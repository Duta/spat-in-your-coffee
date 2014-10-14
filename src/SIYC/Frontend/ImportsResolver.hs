module SIYC.Frontend.ImportsResolver
( resolve'
) where

import SIYC.Frontend.AST
import SIYC.Frontend.Parser
import SIYC.Util

import Control.Exception
import System.Exit
import Text.ParserCombinators.Parsec (ParseError)

resolve
  :: ClassName
  -> IO (Either ParseError [SIYCClass])
resolve name
  = do
    code <- handler name `handle` readFile (name ++ ".siyc")
    case siycParse name code of
      Left e ->
        return $ Left e
      Right file ->
        resolve' file
  where
    handler
      :: ClassName
      -> IOError
      -> IO a
    handler name _
      = do
        putStrLn $ "Couldn't read class " ++ name ++ "'s file"
        exitFailure

resolve'
  :: SIYCFile
  -> IO (Either ParseError [SIYCClass])
resolve' (SIYCFile imports c)
  = resolveAll imports >>= return . fmap (c:)
  where
    resolveAll
      :: [SIYCImport]
      -> IO (Either ParseError [SIYCClass])
    resolveAll []
      = return $ Right []
    resolveAll (SIYCImport name:imports)
      = do
        resolved <- resolve name
        case resolved of
          Left e ->
            return $ Left e
          Right cs -> do
            resolveds <- resolveAll imports
            return $ fmap (cs++) resolveds

-- Jesus this is all ugly. Look into improvements

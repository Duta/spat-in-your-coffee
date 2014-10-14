module SIYC.Frontend.ImportsResolver
( loadAndResolve
) where

import SIYC.Frontend.AST
import SIYC.Frontend.Parser
import SIYC.Util

import Control.Exception
import System.Exit
import Text.ParserCombinators.Parsec (ParseError)

loadAndResolve
  :: ClassName
  -> IO (Either ParseError [SIYCClass])
loadAndResolve
  = loadAndResolve' []

loadAndResolve'
  :: [ClassName]
  -> ClassName
  -> IO (Either ParseError [SIYCClass])
loadAndResolve' loadedNames name
  | name `elem` loadedNames
  = return $ Right []
  | otherwise
  = do
    code <- handler name `handle` readFile (name ++ ".siyc")
    case siycParse name code of
      Left e ->
        return $ Left e
      Right file ->
        resolve (name:loadedNames) file
  where
    handler
      :: ClassName
      -> IOError
      -> IO a
    handler name _
      = do
        putStrLn $ "Couldn't read class " ++ name ++ "'s file"
        exitFailure

resolve
  :: [ClassName]
  -> SIYCFile
  -> IO (Either ParseError [SIYCClass])
resolve loadedNames (SIYCFile imports c)
  = resolveAll imports >>= return . fmap (c:)
  where
    resolveAll
      :: [SIYCImport]
      -> IO (Either ParseError [SIYCClass])
    resolveAll []
      = return $ Right []
    resolveAll (SIYCImport name:imports)
      = do
        resolved <- loadAndResolve' loadedNames name
        case resolved of
          Left e ->
            return $ Left e
          Right cs -> do
            resolveds <- resolveAll imports
            return $ fmap (cs++) resolveds

-- Jesus this is all ugly. Look into improvements

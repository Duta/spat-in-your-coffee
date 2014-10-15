module SIYC.Frontend.ImportsResolver
( loadAndResolve
) where

import SIYC.Frontend.AST
import SIYC.Frontend.Parser
import SIYC.Util

import Control.Exception
import Control.Monad.State
import System.Exit
import Text.ParserCombinators.Parsec (ParseError)

loadAndResolve
  :: ClassName
  -> IO (Either ParseError [SIYCClass])
loadAndResolve name
  = evalStateT (loadAndResolve' name) []

loadAndResolveAll
  :: [ClassName]
  -> IO (Either ParseError [SIYCClass])
loadAndResolveAll names
  = evalStateT (loadAndResolveAll' $ map SIYCImport names) []

loadAndResolve'
  :: ClassName
  -> StateT [ClassName] IO (Either ParseError [SIYCClass])
loadAndResolve' name
  = do
    loadedNames <- get
    if name `elem` loadedNames then
      return $ Right []
    else do
      modify (name:)
      code <- lift $ handler name `handle` readFile (name ++ ".siyc")
      ast <- return $ siycParse name code
      either (return . Left) resolve ast
  where
    handler
      :: ClassName
      -> IOError
      -> IO a
    handler name _
      = do
        putStrLn $ "Couldn't read class " ++ name ++ "'s file"
        exitFailure

loadAndResolveAll'
  :: [SIYCImport]
  -> StateT [ClassName] IO (Either ParseError [SIYCClass])
loadAndResolveAll' []
  = return $ Right []
loadAndResolveAll' (SIYCImport name:imports)
  = do
    resolved <- loadAndResolve' name
    case resolved of
      Left e ->
        return $ Left e
      Right cs -> do
        resolveds <- loadAndResolveAll' imports
        return $ fmap (cs++) resolveds

resolve
  :: SIYCFile
  -> StateT [ClassName] IO (Either ParseError [SIYCClass])
resolve (SIYCFile imports c)
  = loadAndResolveAll' imports >>= return . fmap (c:)

-- Jesus this is all ugly. Look into improvements

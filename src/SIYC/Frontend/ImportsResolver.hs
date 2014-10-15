module SIYC.Frontend.ImportsResolver
( loadAndResolve
, loadAndResolveAll
) where

import SIYC.Frontend.AST
import SIYC.Frontend.Parser
import SIYC.Util

import Control.Conditional ((??))
import Control.Exception
import Control.Monad.State hiding (mapM)
import Data.Traversable (mapM)
import Prelude hiding (mapM)
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
  = get >>= (return (Right []) ?? processFile) . elem name
  where
    processFile
      :: StateT [ClassName] IO (Either ParseError [SIYCClass])
    processFile
     = modify (name:)                                     >>
       lift (handler `handle` readFile (name ++ ".siyc")) >>=
       return . siycParse name                            >>=
       mapM' resolve
    handler
      :: IOError
      -> IO a
    handler
      = const $ do
        putStrLn $ "Couldn't read class " ++ name ++ "'s file"
        exitFailure

loadAndResolveAll'
  :: [SIYCImport]
  -> StateT [ClassName] IO (Either ParseError [SIYCClass])
loadAndResolveAll' []
  = return $ Right []
loadAndResolveAll' (SIYCImport name:imports)
  = loadAndResolve' name >>= mapM' (resolve' imports)

resolve
  :: SIYCFile
  -> StateT [ClassName] IO (Either ParseError [SIYCClass])
resolve (SIYCFile imports c)
  = resolve' imports [c]

resolve'
  :: [SIYCImport]
  -> [SIYCClass]
  -> StateT [ClassName] IO (Either ParseError [SIYCClass])
resolve' imports cs
  = loadAndResolveAll' imports >>= return . fmap (cs++)

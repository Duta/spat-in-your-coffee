module SIYC.Main where

import SIYC.Frontend.ImportsResolver

import System.Environment (getArgs)

main
  :: IO ()
main
  = do
    args <- getArgs
    loadedClasses <- loadAndResolveAll args
    case loadedClasses of
      Left e ->
        putStrLn $ "Parse error in class " ++ show e
      Right classes ->
        print classes

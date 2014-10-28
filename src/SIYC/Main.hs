module SIYC.Main where

import SIYC.ASTChecks.NoUnknownClasses
import SIYC.Backend.CGen
import SIYC.Backend.CPrettyPrinter
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
        case findUnknownClasses classes of
          []  -> mapM_ (uncurry writeFile) . concatMap pp $ map gen classes
          ucs -> putStrLn $ "Unknown classes: " ++ show ucs

{-# OPTIONS_GHC -cpp #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath.Posix
import System.Process


import AbsCPP
import LexCPP
import ParCPP
import ErrM
import TypeChecker
import Compiler


#if __GLASGOW_HASKELL__ >= 806 && __GLASGOW_HASKELL__ < 808
import Control.Monad.Fail (MonadFail(..))
instance MonadFail Err where
  fail = Bad
#endif

runCompiler :: String -> String -> IO () 
runCompiler path s = case pProgram (myLexer s) of
            Bad err  -> do putStrLn "SYNTAX ERROR"
                           putStrLn err
                           exitFailure 
            Ok  tree -> case typecheck tree of
                          Bad err -> do putStrLn "TYPE ERROR"
                                        putStrLn err
                                        exitFailure 
                          Ok p    -> do 
#ifdef mingw32_HOST_OS  
                                        file <- return $ drop 5 (takeBaseName path)
#else
                                        file <- return $ takeBaseName path
#endif
                                        writeFile (file ++ ".j") . unlines . compile file $ p
                                        callCommand $ "java -jar jasmin.jar " ++ file ++ ".j > outtrash.txt"
                                        callCommand $ "java " ++ file
                                        
main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> readFile file >>= runCompiler file
            _      -> do putStrLn "Usage: cpptc <SourceFile>"
                         exitFailure


module Main where

import RunInstant

import System.FilePath
import System.Process
import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import LexInstant
import ParInstant
import SkelInstant
import PrintInstant
import AbsInstant
import Control.Monad.IO.Class
import System.Directory
import Helpers

main :: IO ()
main = do
    args <- getArgs
    curdDir <- getCurrentDirectory
    fPath <- return (head args)
    readFile fPath >>=  run fPath "jvm" 2 pProgram
    liftIO $ readProcess "java" ["-jar", "lib/jasmin.jar", (dropExtension fPath) ++ ".j", "-d", takeDirectory fPath] ""
    return ()

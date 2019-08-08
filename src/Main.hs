module Main where

import Control.Monad.State.Lazy
import Data.List
import System.Environment
import System.IO

-- Make a language where there are distinctions between reals, rationals, complex,
-- integers, etc. but typesafe/typedefs optional, can be specified or inferred, integrate
-- this system with prolog?

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

help :: String
help = "Usage: ./makefile [-option] <source file> <destination file>\n"
    ++ "-option: defaults to \"-css\"\n"
    ++ "\t-t: Print tokens\n"
    ++ "\t-a: Prints AST\n"
    ++ "\t-css: Compile stdin to stdout \n"
    ++ "\t-csf: Compile stdin to file\n"
    ++ "\t-cfs: Compile file to stdout\n"
    ++ "\t-cff: Compile file to file\n"
    ++ "\t-h: Help"

nstrip :: String -> String
nstrip str = filter (/= '\n') str

sstrip :: String -> String
sstrip str = filter (/= ' ') str

tstrip :: String -> String
tstrip str = filter (/= '\t') str

interp :: IO ()
interp = do
    putStr ">> "
    hFlush stdout
    a <- getLine
    let b = nstrip $ sstrip $ tstrip a
    if b == []
        then interp
        else if ":quit" `isInfixOf` b
            then putStr ""      
            else do { putStrLn b
                    ; interp
                    }

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-h"]              -> putStrLn help
        ["-t"]              -> putStrLn "o"
        ["-css"]            -> interp
        [cmd]               -> putStrLn $ sstrip cmd
        [cmd, path]         -> putStrLn $ sstrip path
        [cmd, src, dest]    -> putStrLn $ sstrip dest
        _                   -> putStrLn help
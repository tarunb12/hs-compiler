module Main where

import System.Environment

help :: String
help = "Usage: ./makefile [-option] <source file> <destination file>\n"
    ++ "-option: defaults to \"-css\""
    ++ "\t-t: Print tokens\n"
    ++ "\t-a: Prints AST\n"
    ++ "\t-css: Compile stdin to stdout \n"
    ++ "\t-csf: Compile stdin to file\n"
    ++ "\t-cfs: Compile file to stdout\n"
    ++ "\t-cff: Compile file to file\n"
    ++ "\t-h: Help"

strip :: String -> String
strip str = filter (/= ' ') str

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["-h"]              -> putStrLn "o"
        [cmd]               -> putStrLn $ strip cmd
        [cmd, path]         -> putStrLn $ strip path
        [cmd, src, dest]   -> putStrLn $ strip dest
        _                   -> putStrLn help
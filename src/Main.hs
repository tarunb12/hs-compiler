module Main where

import System.Environment

help :: String
help = "Usage:\n"
    ++ "Arg list"

main :: IO ()
main = do
    args <- getArgs
    case args of
        _   -> putStrLn help
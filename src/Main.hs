module Main where

import Interpret(run)

import Parser (Mode(..))

import System.Environment
import System.IO

-- Make a language where there are distinctions between reals, rationals, complex,
-- integers, etc. but typesafe/typedefs optional, can be specified or inferred, integrate
-- this system with prolog?

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"]              -> run $ Help
    ["-t", path]        -> run $ TokenPrint path
    ["-a", path]        -> run $ AstPrint path
    ["-css"]            -> run $ StdinStdout
    ["-csf", path]      -> run $ StdinFile path
    ["-cfs", path]      -> run $ FileStdout path
    ["-cff", src, dest] -> run $ FileFile src dest
    _                   -> run $ Help
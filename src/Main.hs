module Main where

import Interpret (run)

import Parser (Mode(..))

import System.Environment
import System.IO

-- Make a language where there are distinctions between reals, rationals, complex,
-- integers, etc. but typesafe/typedefs optional, can be specified or inferred, integrate
-- this system with prolog?

main :: IO ()
main = do
  args <- getArgs
  run $ case args of
    ["-h"]              -> Help
    ["-t", path]        -> TokenPrint path
    ["-a", path]        -> AstPrint path
    ["-css"]            -> StdinStdout
    ["-csf", path]      -> StdinFile path
    ["-cfs", path]      -> FileStdout path
    ["-cff", src, dest] -> FileFile src dest
    _                   -> Help
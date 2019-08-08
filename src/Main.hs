module Main
  ( Mode(..)
  , main
  ) where

import Data.List
-- import Parser
import System.Environment
import System.IO

-- Make a language where there are distinctions between reals, rationals, complex,
-- integers, etc. but typesafe/typedefs optional, can be specified or inferred, integrate
-- this system with prolog?

data Mode
  = Help
  | TokenPrint String
  | AstPrint String
  | StdinStdout
  | StdinFile String
  | FileStdout String
  | FileFile String String

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

interpStdout :: IO ()
interpStdout = do
  putStr ">> "
  hFlush stdout
  a <- getLine
  let b = nstrip $ sstrip $ tstrip a
  if b == []
    then interpStdout
    else if ":quit" `isInfixOf` b
      then return ()    
      else do { putStrLn b
              ; interpStdout
              }

-- interpFile :: String -> IO ()
-- interpFile dest =

-- compileStdout :: String -> IO ()
-- compileStdout src =

-- compileFile :: String -> String -> IO ()
-- compileFile src dest =

run :: Mode -> IO ()
run Help            = putStrLn help
run (TokenPrint f)  = putStrLn "tokens"
run (AstPrint f)    = putStrLn "ast"
run StdinStdout     = interpStdout
run (StdinFile d)   = interpStdout
run (FileStdout s)  = interpStdout
run (FileFile s d)  = interpStdout
run _               = putStrLn help

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
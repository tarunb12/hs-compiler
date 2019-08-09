module Main where

import Data.List
import Parser(Mode(..))
import System.Environment
import System.IO

-- Make a language where there are distinctions between reals, rationals, complex,
-- integers, etc. but typesafe/typedefs optional, can be specified or inferred, integrate
-- this system with prolog?

help :: String
help = "Usage: ./makefile [-option] <source file?> <destination file?>\n"
  ++ "-option: defaults to \"-css\"\n"
  ++ "\t-t: Print tokens\n"
  ++ "\t-a: Prints AST\n"
  ++ "\t-css: Compile stdin to stdout \n"
  ++ "\t-csf: Compile stdin to file\n"
  ++ "\t-cfs: Compile file to stdout\n"
  ++ "\t-cff: Compile file to file\n"
  ++ "\t-h: Help"

wstrip :: String -> String
wstrip str = filter (not . flip elem "\n\t ") str

interpret :: Mode -> IO ()
interpret StdinStdout = do
  putStr "λ> "
  hFlush stdout
  a <- getLine
  let b = wstrip a
  if b == []
    then interpret StdinStdout
    else if ":quit" `isInfixOf` b
      then return ()    
      else do { putStrLn b
              ; interpret StdinStdout
              }

interpret (StdinFile f) = do
  putStr ""
  
-- interpFile :: String -> IO ()
-- interpFile dest =

-- compileStdout :: String -> IO ()
-- compileStdout src =

-- compileFile :: String -> String -> IO ()
-- compileFile src dest =

run :: Mode -> IO ()
run (Help)          = putStrLn help
run (TokenPrint f)  = putStrLn "tokens"
run (AstPrint f)    = putStrLn "ast"
run (StdinStdout)   = interpret StdinStdout
run (StdinFile d)   = interpret StdinStdout
run (FileStdout s)  = interpret StdinStdout
run (FileFile s d)  = interpret StdinStdout

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
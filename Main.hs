module Main where

import Colourista
import Control.Monad (unless, when)
import qualified Data.Text as Text
import Module.OPG_Parser (parseCheck, parseOPGfile, unwrap)
import Module.OPG_Printer (output, readLines)
import Module.OPG_core
  ( Grammar,
    Matrix,
    Relation (..),
    output_matrix,
    output_t,
    reify,
    type_checker,
  )
import System.Directory (doesFileExist)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

compute :: String -> IO ()
compute path =
  if unsafePerformIO (doesFileExist path)
    then do
      let grammar_raw = unsafePerformIO (readLines path)
      let grammar = parseOPGfile grammar_raw
      if parseCheck grammar
        then do
          successMessage $ Text.pack "Grammar loaded."
          output (path ++ "_output.md") (unwrap grammar)
        else errorMessage $ Text.pack "Grammar parse failed!"
    else errorMessage $ Text.pack "File does not exist!"

main :: IO ()
main = do
  printf (formatWith [yellowBg, magenta, bold] "Input the grammar file: ")
  hFlush stdout
  path <- getLine
  when (path == "test") $ do
    infoMessage $ Text.pack "Test 1: accepted grammar"
    compute "./Test/in1.txt"
    infoMessage $ Text.pack "Test 2: Unaccepted grammar (Error 2)"
    compute "./Test/in2.txt"
    infoMessage $ Text.pack "Test 3: Unaccepted grammar (Error 1)"
    compute "./Test/in3.txt"
    successMessage $ Text.pack "All tests passed."
    main
  when (path == ":q") (printf (formatWith [bold] "Bye."))
  unless (path == ":q" || path == "test") $ do
    compute path
    main

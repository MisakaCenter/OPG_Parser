module Main where

import Colourista
import Control.Monad (unless, when)
import qualified Data.Text as Text
import Module.OPG_Parser (parseCheck, parseOPGfile, unwrap)
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

outputHeadLine :: [String] -> IO ()
outputHeadLine [] = printf " "
outputHeadLine (x : xs) = do
  printf "%s " x
  outputHeadLine xs

outputRelation :: Relation -> IO ()
outputRelation Le = printf "%s" "<"
outputRelation Ge = printf "%s" ">"
outputRelation Emp = printf "%s" " "
outputRelation Eq = printf "%s" "="

outputMiddleLine :: [Relation] -> IO ()
outputMiddleLine [] = printf " "
outputMiddleLine (x : xs) = do
  outputRelation x
  printf " "
  outputMiddleLine xs

outputResult :: ([String], Matrix) -> IO ()
outputResult sm =
  case sm of
    ([], []) -> printf "\n"
    (x : xs, l : xl) -> do
      printf "%s " x
      outputMiddleLine l
      printf "\n"
      outputResult (xs, xl)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

checkResult :: [[Relation]] -> Bool
checkResult m =
  case m of
    [] -> True
    x : xs -> case x of
      [] -> False
      _ -> checkResult xs

output :: Grammar -> IO ()
output g = do
  let matrix = output_matrix g
  let t = output_t g
  let acc = type_checker (reify g)
  let check = checkResult matrix
  when (check && acc) $ do
    outputHeadLine (" " : t)
    printf "\n"
    outputResult (t, matrix)
  unless acc (errorMessage $ Text.pack "Grammar is not acceptable(1). Please input Operator Precedence Grammar!")
  unless check (errorMessage $ Text.pack "Grammar is not acceptable(2). Please input Operator Precedence Grammar!")

main :: IO ()
main = do
  printf (formatWith [yellowBg, magenta, bold] "Input the grammar file: ")
  hFlush stdout
  path <- getLine
  when (path == "test") $ do
    infoMessage $ Text.pack "Test 1: accepted grammar"
    output (unwrap (parseOPGfile (unsafePerformIO (readLines "./Test/in1.txt"))))
    infoMessage $ Text.pack "Test 2: Unaccepted grammar (Error 2)"
    output (unwrap (parseOPGfile (unsafePerformIO (readLines "./Test/in2.txt"))))
    infoMessage $ Text.pack "Test 3: Unaccepted grammar (Error 1)"
    output (unwrap (parseOPGfile (unsafePerformIO (readLines "./Test/in3.txt"))))
    successMessage $ Text.pack "All tests passed."
    main
  unless (path == ":q" || path == "test") $ do
    when (unsafePerformIO (doesFileExist path)) $ do
      let grammar_raw = unsafePerformIO (readLines path)
      let grammar = parseOPGfile grammar_raw
      when (parseCheck grammar) $ do
        successMessage $ Text.pack "Grammar loaded."
        output (unwrap grammar)
      unless (parseCheck grammar) (errorMessage $ Text.pack "Grammar parse failed!")
    unless (unsafePerformIO (doesFileExist path)) (errorMessage $ Text.pack "File does not exist!")
    main
  when (path == ":q") (printf (formatWith [bold] "Bye."))
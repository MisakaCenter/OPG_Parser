module Module.OPG_Printer where

import Colourista
import Control.Monad (unless, when)
import qualified Data.Text as Text
import Module.OPG_FileIO (output_f)
import Module.OPG_core
  ( Grammar,
    Matrix,
    Relation (..),
    output_matrix,
    output_t,
    reify,
    type_checker,
  )
import Text.Printf (printf)

outputHeadLine :: [String] -> IO ()
outputHeadLine [] = printf "|\n"
outputHeadLine (x : xs) = do
  printf "| %s " x
  outputHeadLine xs

outputFormatLine :: Int -> IO ()
outputFormatLine 0 = printf "|\n"
outputFormatLine n = do
  printf "|---"
  outputFormatLine (n - 1)

outputRelation :: Relation -> IO ()
outputRelation Le = printf "<"
outputRelation Ge = printf ">"
outputRelation Emp = printf " "
outputRelation Eq = printf "="

outputMiddleLine :: [Relation] -> IO ()
outputMiddleLine [] = printf "|\n"
outputMiddleLine (x : xs) = do
  printf "| "
  outputRelation x
  printf " "
  outputMiddleLine xs

outputResult :: ([String], Matrix) -> IO ()
outputResult sm =
  case sm of
    ([], []) -> printf ""
    (x : xs, l : xl) -> do
      printf "| %s " x
      outputMiddleLine l
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

output :: String -> Grammar -> IO ()
output f g = do
  let matrix = output_matrix g
  let t = output_t g
  let acc = type_checker (reify g)
  let check = checkResult matrix
  when (check && acc) $ do
    outputHeadLine (" " : t)
    outputFormatLine (Prelude.length t + 1)
    outputResult (t, matrix)
    successMessage $ Text.pack ("Operator Precedence Analysis Table is generated. Location: (" ++ f ++ ")")
    output_f f g
  unless acc (errorMessage $ Text.pack "Grammar is not acceptable(1). Please input Operator Precedence Grammar!")
  unless check (errorMessage $ Text.pack "Grammar is not acceptable(2). Please input Operator Precedence Grammar!")

module Module.OPG_FileIO where
import System.IO
import Control.Monad (unless, when)
import Module.OPG_core
  ( Grammar,
    Matrix,
    Relation (..),
    output_matrix,
    output_t,
  )

outputHeadLine_f :: Handle  -> [String] -> IO ()
outputHeadLine_f f [] = hPutStr f "|\n"
outputHeadLine_f f (x : xs) = do
  hPutStr f ("| " ++ x ++ " ")
  outputHeadLine_f f xs

outputFormatLine_f :: Handle -> Int -> IO ()
outputFormatLine_f f 0 = hPutStr f "|\n"
outputFormatLine_f f n = do
  hPutStr f "|---"
  outputFormatLine_f f (n - 1)

outputRelation_f :: Handle -> Relation -> IO ()
outputRelation_f f Le = hPutStr f "<"
outputRelation_f f Ge = hPutStr f ">"
outputRelation_f f Emp = hPutStr f " "
outputRelation_f f Eq = hPutStr f "="

outputMiddleLine_f :: Handle -> [Relation] -> IO ()
outputMiddleLine_f f [] = hPutStr f "|\n"
outputMiddleLine_f f (x : xs) = do
  hPutStr f "| "
  outputRelation_f f x
  hPutStr f " "
  outputMiddleLine_f f xs

outputResult_f :: Handle -> ([String], Matrix) -> IO ()
outputResult_f f sm =
  case sm of
    ([], []) -> hPutStr f ""
    (x : xs, l : xl) -> do
      hPutStr f ("| " ++ x ++ " ")
      outputMiddleLine_f f l
      outputResult_f f (xs, xl)

output_f :: String -> Grammar -> IO ()
output_f f g = do
  handle <- openFile f WriteMode
  let matrix = output_matrix g
  let t = output_t g
  outputHeadLine_f handle (" " : t)
  outputFormatLine_f handle (Prelude.length t + 1)
  outputResult_f handle (t, matrix)
  hClose handle
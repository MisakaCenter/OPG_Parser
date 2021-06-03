module Main where
import OPG_core
    ( Matrix, Relation(..), Grammar, output_t, output_matrix )
import OPG_Parser ( parseOPGfile, unwrap, parseCheck )
import Control.Monad ( when, unless )
import System.IO ( stdout, hFlush )
import System.IO.Unsafe ( unsafePerformIO )
import Text.Printf ( printf )
import System.Directory ( doesFileExist )

outputHeadLine :: [String] -> IO()
outputHeadLine [] = printf " "
outputHeadLine (x:xs) = do
    printf "%s " x
    outputHeadLine xs

outputRelation :: Relation -> IO()
outputRelation Le = printf "%s" "<"
outputRelation Ge = printf "%s" ">"
outputRelation Emp = printf "%s" " "
outputRelation Eq = printf "%s" "="

outputMiddleLine :: [Relation] -> IO()
outputMiddleLine [] = printf " "
outputMiddleLine (x:xs) = do
    outputRelation x
    printf " "
    outputMiddleLine xs

outputResult :: ([String], Matrix) -> IO()
outputResult sm =
    case sm of{
        ([], []) -> printf "\n";
        (x:xs, l:xl) -> do
                            printf "%s " x
                            outputMiddleLine l
                            printf "\n"
                            outputResult (xs,xl)
    }

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

output :: Grammar -> IO()
output g = do outputHeadLine (" ":output_t g)
              printf "\n"
              outputResult (output_t g, output_matrix g)

main :: IO()
main = do
          putStr "Input the grammar file: "
          hFlush stdout
          path <- getLine
          unless (path == ":q") $ do
            when (unsafePerformIO(doesFileExist path)) $ do
                let grammar_raw = unsafePerformIO(readLines path)
                let grammar = parseOPGfile grammar_raw
                when (parseCheck grammar) $ do
                    printf "[Log] Grammar loaded.\n"
                    output (unwrap grammar)
                unless (parseCheck grammar) 
                    (printf "[Error] Grammar parse failed!\n")
            unless (unsafePerformIO(doesFileExist path)) (printf "[Error] File does not exist!\n")
            main
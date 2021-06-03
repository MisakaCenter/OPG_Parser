module OPG_Parser where
import OPG_core ( Rule, Expr(..), Symbol(U) )
import Text.ParserCombinators.Parsec
    ( char,
      ParseError,
      noneOf,
      space,
      string,
      many1,
      (<|>),
      many,
      parse,
      skipMany,
      GenParser )

exprsToExpr :: [Expr] -> Expr 
exprsToExpr = foldl1 Combine

line :: GenParser Char st Rule
line =
    do constr <- symbolContent
       skipMany space
       string "->"
       skipMany space
       result <- exprs
       let expr = exprsToExpr result
       return (constr,  expr)

exprs :: GenParser Char st [Expr]
exprs =
    do skipMany space
       first <- exprContent
       skipMany space
       next <- remainingExpr
       return (first : next)

remainingExpr :: GenParser Char st [Expr]
remainingExpr =
    (char '|' >> exprs)
    <|> return []

exprContent :: GenParser Char st Expr
exprContent =
    do l <- many symbolContent
       return (Base l)

symbolContent :: GenParser Char st Symbol
symbolContent = 
    do first <- many1 (noneOf "->|\n ")
       skipMany space
       return (U first)

eol :: GenParser Char st Char
eol = char '\n'

parseOPG :: String -> Either ParseError Rule
parseOPG = parse line "(unknown)"

parseOPGfile :: [String] -> Either ParseError [Rule]
parseOPGfile [] = Right []
parseOPGfile (x:xs) = case parseOPG x of {
                Left err -> Left err;
                Right p -> case parseOPGfile xs of {
                                  Left err -> Left err;
                                  Right p1 -> Right (p : p1)
                              }
             }

unwrap :: Either ParseError [Rule] -> [Rule]
unwrap x = case x of {
    Left _ -> [];
    Right r -> r
}

parseCheck :: Either ParseError [Rule] -> Bool 
parseCheck x = case x of {
    Left _ -> False;
    Right _ -> True
}
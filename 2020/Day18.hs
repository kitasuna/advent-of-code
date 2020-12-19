module Main where

import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Token
import           Text.Parsec.Language           ( haskellStyle )
import           Data.Functor.Identity

lexer :: GenTokenParser String u Identity
lexer = makeTokenParser haskellStyle

expr :: ParsecT String u Identity Integer
expr = buildExpressionParser tablePt2 term <?> "expression"

term :: ParsecT String u Identity Integer
term = parens lexer expr <|> natural lexer <?> "simple expression"

tablePt1 :: [[Operator String u Identity Integer]]
tablePt1 = [[binary "*" (*) AssocLeft, binary "+" (+) AssocLeft]]

tablePt2 :: [[Operator String u Identity Integer]]
tablePt2 = [[binary "+" (+) AssocLeft], [binary "*" (*) AssocLeft]]

binary :: String -> (a -> a -> a) -> Assoc -> Operator String u Identity a
binary name fun assoc = Infix
  (do
    reservedOp lexer name
    return fun
  )
  assoc

parseExp :: String -> Either ParseError Integer
parseExp = parse expr ""

f :: [Either a Integer] -> Integer
f ms = foldr
  (\m -> \acc -> case m of
    Right i -> acc + i
    Left  _ -> acc
  )
  0
  ms
main :: IO ()
main = do
  input <- readFile "Day18.input"
  print $ show $ f (fmap parseExp (lines input))

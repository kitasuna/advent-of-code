module Main where

import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Token
import           Text.Parsec.Language           ( haskellStyle )

lexer = makeTokenParser haskellStyle

expr = buildExpressionParser tablePt2 term <?> "expression"

term = parens lexer expr <|> natural lexer <?> "simple expression"

table = [[binary "*" (*) AssocLeft, binary "+" (+) AssocLeft]]

tablePt2 = [[binary "+" (+) AssocLeft], [binary "*" (*) AssocLeft]]

binary name fun assoc = Infix
  (do
    reservedOp lexer name
    return fun
  )
  assoc

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

module Day04 where

import qualified Data.Map                      as M
import           Text.ParserCombinators.Parsec

foldDatMap :: M.Map String String -> Maybe String
foldDatMap m =
  M.lookup "byr" m
    >> M.lookup "iyr" m
    >> M.lookup "eyr" m
    >> M.lookup "hgt" m
    >> M.lookup "hcl" m
    >> M.lookup "ecl" m
    >> M.lookup "pid" m

countMaybes :: [Maybe String] -> Integer
countMaybes ms = foldr
  (\m -> \x -> case m of
    Just val -> x + 1
    Nothing  -> x
  )
  0
  ms

dropEmpties :: [[(String, String)]] -> [[(String, String)]]
dropEmpties kvs = filter (\xs -> length kvs > 0) kvs

file :: GenParser Char str Integer
file = do
  lines         <- line `sepBy` (char '\n')
  passportCount <-
    return $ countMaybes $ fmap (foldDatMap . M.fromList) $ dropEmpties lines
  eof
  return passportCount

line :: GenParser Char str [(String, String)]
line = do
  kvs <- many kv
  return kvs

kv :: GenParser Char str (String, String)
kv = do
  key <- many alphaNum
  char ':'
  val <- many (alphaNum <|> (char '#'))
  char ' ' <|> char '\n'
  return (key, val)

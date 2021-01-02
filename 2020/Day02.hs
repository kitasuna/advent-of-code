module Main where

import           Text.ParserCombinators.Parsec
import Data.Char (digitToInt)
import Data.Either (rights)
import Text.Parsec

main :: IO ()
main = do 
  text <- readFile "Day02.input"
  let inj = \l -> (parse line "") l >>= pwIsValid
      part1 = length ( rights ( fmap inj $ lines text) )
  print ">>> DAY 02 <<<"
  print $ "Part 1: " ++ show part1

data Pwr = Pwr {
  minC :: Int,
  maxC :: Int,
  tgt :: Char,
  pw :: String
               } deriving (Eq, Show)

countChar :: Char -> String -> Int
countChar c s = length $ filter (== c) s

pwIsValid :: Pwr -> Either ParseError String
pwIsValid pwr = if tally >= (minC pwr) && tally <= (maxC pwr) then Right (pw pwr) else (parse (unexpected (pw pwr)) "" "")
  where tally = countChar (tgt pwr) (pw pwr)

line :: Parsec String () Pwr
line = do
  min <- many digit
  char '-'
  max <- many digit
  spaces
  tgt <- alphaNum
  char ':'
  spaces
  password <- many alphaNum
  eof
  return Pwr { minC = read min, maxC = read max, tgt = tgt, pw = password }
  

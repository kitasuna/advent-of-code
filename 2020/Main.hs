module Main where

import Text.ParserCombinators.Parsec
import Day04

main :: IO ()
main = do
  text <- readFile "Day04.input"
  print $ show $ parse file "g" text
-- main = putStrLn $ show $ parseInput testInput

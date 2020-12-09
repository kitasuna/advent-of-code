module Main where

-- import Day07
import Day08
import System.IO

main :: IO ()
-- main = putStrLn $ show $ fmap instsToTasks $ parseInput testInput
main = do
  input <- readFile "input08"
  print $ show $ fmap sum $ fmap getMeta $ parseInput08 input

module Main where

import qualified Data.Map as M 

tupleUp :: a -> (a, Bool)
tupleUp = ( flip (,) ) True 

type Target = Integer

f :: M.Map Integer Bool -> Target -> Integer -> Maybe Integer
f m t x = if M.member (t - x) m then Just (x * (t - x)) else Nothing

g :: M.Map Integer Bool -> Target -> (Integer, Integer) -> Maybe Integer
g m t pair = if M.member (t - pairSum) m then Just (pairProduct * (t - pairSum)) else Nothing
  where pairSum = (fst pair) + (snd pair)
        pairProduct = (fst pair) * (snd pair)

pairs :: [Integer] -> [(Integer, Integer)]
pairs [] = []
pairs (x:[]) = []
pairs (x:xs) = (pairs' x xs) ++ (pairs xs)

pairs' :: Integer -> [Integer] -> [(Integer, Integer)]
pairs' _ [] = []
pairs' x (y:ys) = [(x, y)] ++ (pairs' x ys)

main :: IO ()
main = do
  input <- readFile "Day01.input"
  let numbers = fmap read (lines input) :: [Integer]
      mapped = M.fromList $ fmap tupleUp numbers 
      maybes = fmap (f mapped 2020) numbers
      maybes2 = fmap (g mapped 2020) (pairs numbers)
      part1 = foldr (\m -> \x -> case m of 
                                     Just val -> val
                                     Nothing -> x
                      ) 0 maybes
      part2 = foldr (\m -> \x -> case m of 
                                     Just val -> val
                                     Nothing -> x
                      ) 0 maybes2


  print ">>> DAY 01 <<<"
  print $ "Part 1: " ++ show part1
  print $ "Part 2: " ++ show part2

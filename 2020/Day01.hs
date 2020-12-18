module Main where

import qualified Data.Map as M 

tupleUp :: a -> (a, Bool)
tupleUp = ( flip (,) ) True 

f :: M.Map Integer Bool -> Integer -> Maybe Integer
f m x = if M.member (2020 - x) m then Just (x * (2020 - x)) else Nothing

main :: IO ()
main = do
  input <- readFile "Day01.input"
  let numbers = fmap read (lines input) :: [Integer]
      mapped = M.fromList $ fmap tupleUp numbers 
      maybes = fmap (f mapped) numbers
      final = foldr (\m -> \x -> case m of 
                                     Just val -> val
                                     Nothing -> x
                      ) 0 maybes

  print ">>> DAY 01 <<<"
  print $ "Part 1: " ++ show final

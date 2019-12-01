{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List

main :: IO ()
main = do
  input :: [Int] <- fmap read . lines <$> readFile "input.txt"
  -- Part 1
  print $ sum (f <$> input)
  -- Part 2
  print $ sum (accum <$> input)

-- | Part 1
f :: Int -> Int
f = subtract 2 . (`div` 3)

-- | Part 2
accum :: Int -> Int
accum = sum . takeWhile (>=0) . iterate f

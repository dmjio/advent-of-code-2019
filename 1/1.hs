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
f mass = floor (fromIntegral mass / 3) - 2

-- | Part 2
accum :: Int -> Int
accum = sum . unfoldr go
  where
    go x | f x <= 0  = Nothing
         | otherwise = Just (x, f x)

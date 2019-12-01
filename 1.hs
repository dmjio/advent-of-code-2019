{-# LANGUAGE ScopedTypeVariables #-}
module Main where

main :: IO ()
main = do
  input :: [Int] <- fmap read . lines <$> readFile "input.txt"
  -- part 1
  print $ sum (fmap f input)
  -- part 2
  print $ sum (fmap (sum . accum) input)

-- | Part 1
f :: Int -> Int
f mass = floor (fromIntegral mass / 3) - 2

-- | Part 2
accum :: Int -> [Int]
accum x | f x <= 0 = []
        | otherwise = f x : accum (f x)

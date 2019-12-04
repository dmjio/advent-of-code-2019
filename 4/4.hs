{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE BangPatterns        #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Function
import           Data.List
import           Data.List.Split
import qualified Data.Set            as S
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

import Data.List

main :: IO ()
main = do
  let xs = [ 128392 .. 643281 ]
  print
    $ length
    $ filter (\x -> increasing x && adjacentIdentical x) xs
  print
    $ length
    $ filter adjacentGrouped xs

increasing :: Int -> Bool
increasing
  (show -> [digitToInt -> a, digitToInt -> b, digitToInt -> c, digitToInt -> d, digitToInt -> e, digitToInt -> f]) =
    a <= b && b <= c && c <= d && d <= (e :: Int) && e <= f

adjacentIdentical :: Int -> Bool
adjacentIdentical
  (show -> [digitToInt -> a, digitToInt -> b, digitToInt -> c, digitToInt -> d, digitToInt -> e, digitToInt -> f]) =
    a == b || b == c || c == d || d == (e :: Int) || e == f

adjacentGrouped :: Int -> Bool
adjacentGrouped xs = do
  let grouped = group (show xs)
  let allIncreasing = increasing' $ map (read . pure . head) grouped
      containsTwo = any ((==2) . length) grouped
  containsTwo && allIncreasing
    where
      increasing' :: [Int] -> Bool
      increasing' xs = and $ zipWith (<=) xs (tail xs)


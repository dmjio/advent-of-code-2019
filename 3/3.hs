{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import           Control.Applicative
import           Control.Monad

import           Data.Function
import           Data.List
import           Data.List.Split

import           Data.Vector         (Vector)
import qualified Data.Vector         as V

import qualified Data.Set            as S

main :: IO ()
main = do
  [first, second] <-
      fmap (fmap parse)
    . map (splitOn ",")
    . lines <$> readFile "input.txt"

  print (minDist first second)
  print (minSteps first second)

parseLine :: String -> [(Dir, Int)]
parseLine xs = fmap parse (splitOn "," xs)

data Dir = R | L | U | D
  deriving (Show, Eq, Read)

parse :: String -> (Dir,Int)
parse xs = ( read [head xs] , read (tail xs) )

minSteps
  :: (Ord a, Num a)
  => [(Dir, Int)]
  -> [(Dir, Int)]
  -> a
minSteps x y = calcSteps (allMins x y) (buildPath x) (buildPath y)
  where
    findSteps needle haystack = go needle haystack 0
      where
        go n (h:hs) i | n == h = i
                      | otherwise = go n hs (i + 1)

    calcSteps mins first second =
      minimum
      [ findSteps m first + findSteps m second
      | m <- mins
      ]

    allMins first second =
      filter (/= (0,0))
        $ S.toList
        $ S.fromList (buildPath first)
            `S.intersection`
                S.fromList (buildPath second)

minDist first second =
  minimum
    $ fmap manhattan
    $ filter (/= (0,0))
    $ S.toList
    $ S.fromList (buildPath first)
        `S.intersection`
            S.fromList (buildPath second)
  where
    manhattan :: Num a => (a, a) -> a
    manhattan (x,y) = abs x + abs y

buildPath :: [(Dir,Int)] -> [(Int,Int)]
buildPath xs = fst $ foldl' go ([], (0,0)) xs
  where
    go (xs, (x,!y)) (U,k) =
      (xs ++ [ (x,y + i) | i <- [ 0 .. k - 1 ] ], (x, y+k))
    go (xs, (x,!y)) (D,k) =
      (xs ++ [ (x,y - i) | i <- [ 0 .. k - 1 ] ], (x, y-k))
    go (xs, (!x,y)) (R,k) =
      (xs ++ [ (x+i,y)   | i <- [ 0 .. k - 1 ] ], (x+k, y))
    go (xs, (!x,y)) (L,k) =
      (xs ++ [ (x-i,y)   | i <- [ 0 .. k - 1 ] ], (x-k, y))

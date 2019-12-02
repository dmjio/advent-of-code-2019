{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Applicative
import           Control.Monad

import           Data.Function
import           Data.List
import           Data.List.Split

import qualified Data.Vector         as V

main :: IO ()
main = do
  input <-
    V.fromList
      . map read
      . splitOn "," <$> readFile "input.txt"
  let inp = set 1 12 . set 2 2 $ input
  print $ get 0 (go 0 inp)

  let range = [0..99]
  forM_ (liftA2 (,) range range) $ \(noun,verb) -> do
    let inp = set 1 noun . set 2 verb $ input
        result = go 0 inp
    when (get 0 result == 19690720) $ do
      print ((100 * noun) + verb)

-- | Part 1
go ip input =
  case get ip input of
    99 -> input
    1  -> dispatch ip input (+)
    2  -> dispatch ip input (*)
    _  -> error "impossible"

dispatch ip input opCode = do
  let x = get (ip + 1) input
      y = get (ip + 2) input
      p = get (ip + 3) input
      valX = get x input
      valY = get y input
  go (ip + 4) $ set p (valX `opCode` valY) input

get :: Int -> V.Vector Int -> Int
get = flip (V.!)

set :: Int -> Int -> V.Vector Int -> V.Vector Int
set idx val input = input V.// [(idx,val)]

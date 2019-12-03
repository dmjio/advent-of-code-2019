{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Applicative
import           Control.Monad

import           Data.Function
import           Data.List
import           Data.List.Split

import qualified Data.Vector         as V
import           Data.Vector         (Vector)

main :: IO ()
main = do
  input :: Vector Int <-
      V.fromList . map read . splitOn "," <$> readFile "input.txt"
  print input

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.List.Split
import           Data.Monoid
import qualified Data.Set            as S
import qualified Data.Vector         as V
import           Debug.Trace

import qualified Data.Map            as M
import           Data.Maybe
import           Data.Trie
import           Prelude             hiding (lookup, null)

main :: IO ()
main = do
  xs <-
    fmap (\[x,y] -> (x, y)) . fmap (splitOn ")") .
      lines <$> readFile "input.txt"

  let adj = buildMap xs mempty

  -- part 2
  print $
    countEdges
      (buildPath adj "COM" "YOU")
      (buildPath adj "COM" "SAN")

  -- part 1
  print
    $ sum
    $ fmap (countOrbits (buildMap xs mempty) 0)
    $ filter (/="COM")
    $ nub
    $ (\(x,y) -> [x,y]) =<< xs

buildMap
  :: [(String,String)]
  -> Adj
  -> Adj
buildMap [] x = x
buildMap ((k,v):xs) adj = do
  case M.lookup k adj of
    Nothing ->
      buildMap xs $
        M.insert k [v] adj
    Just x ->
      buildMap xs $
        M.insertWith (++) k [v] adj

type Adj = M.Map String [String]

countOrbits :: Adj -> Int -> String -> Int
countOrbits adj orbits xs = do
  let Just vals = M.lookup "COM" adj
  maximum $ dfs xs 1 <$> vals
    where
      dfs :: String -> Int -> String -> Int
      dfs target idx val
        | target == val = idx
        | otherwise = do
            case M.lookup val adj of
              Nothing -> 0
              Just vals ->
                maximum $
                  fmap (dfs target (idx + 1)) vals


buildPath :: Adj -> String -> String -> [String]
buildPath adj start target = maximum (go [] <$> adj M.! start)
  where
    go :: [String] -> String -> [String]
    go path x
      | x == target = path
      | otherwise =
          case M.lookup x adj of
            Nothing -> []
            Just vals -> do
              maximum $
                go (path ++ [x]) <$> vals

countEdges :: [String] -> [String] -> Int
countEdges [] [] = 0
countEdges xxs@(x:xs) yys@(y:ys)
  | x == y = countEdges xs ys
  | otherwise = length (xxs <> yys)

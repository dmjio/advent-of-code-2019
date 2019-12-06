{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import           Control.Applicative
import           Control.Monad

import           Data.Function
import           Data.List
import           Data.List.Split
import           System.IO.Unsafe

import qualified Data.Vector         as V

main :: IO ()
main = do
  input <- V.fromList . map read . splitOn "," <$> readFile "input5.txt"
  void $ go 0 input

-- | day 5
parseOpCode :: Int -> Int
parseOpCode (read . reverse . take 2 . reverse . show -> x) = x

getParamModes
  :: Int
  -> (Bool, Bool, Bool)
getParamModes x | x < 100 = (False,False,False)
getParamModes (read @Int . reverse . drop 2 . reverse . show -> x) =
  case show x of
    [] -> (False,False,False)
    [x] -> (x=='1',False,False)
    [y,x] -> (x=='1',y=='1',False)
    [z,y,x] -> (x=='1',y=='1',z=='1')

debug :: Bool
debug = True

-- | Part 1
go :: Int -> V.Vector Int -> IO (V.Vector Int)
go ip input = do
  let inp = get ip input
  case parseOpCode $ get ip input of
    99 -> pure input
    1  -> dispatch ip input (getParamModes inp) (+)
    2  -> dispatch ip input (getParamModes inp) (*)
    3  -> dispatch3 ip input
    4  -> dispatch4 ip input (getParamModes inp)
    5  -> dispatchJmp ip input (getParamModes inp) (/=)
    6  -> dispatchJmp ip input (getParamModes inp) (==)
    7  -> dispatchCmp ip input (getParamModes inp) (<)
    8  -> dispatchCmp ip input (getParamModes inp) (==)
    _  -> error "impossible"

-- | dispatchCmp
dispatchCmp ip input (xImm, yImm, _) op = do
  let x = get (ip + 1) input
      y = get (ip + 2) input
      p = get (ip + 3) input
      valX = if xImm then x else get x input
      valY = if yImm then y else get y input
  if valX `op` valY
    then go (ip + 4) $ set p 1 input
    else go (ip + 4) $ set p 0 input

-- | jmp-if-true
dispatchJmp ip input (xImm, yImm, _) op = do
  let x = get (ip + 1) input
      y = get (ip + 2) input
      valX = if xImm then x else get x input
      valY = if yImm then y else get y input
  if valX `op` 0
    then go valY input
    else go (ip+3) input

dispatch3 ip input = do
  let addr = get (ip + 1) input
  putStrLn "Input value..."
  v :: Int <- read <$> getLine
  go (ip + 2) $ set addr v input

dispatch4 ip input (xImm,_,_) = do
  let addr = get (ip + 1) input
  let val = get addr input
  print (if xImm then addr else val)
  go (ip + 2) input

dispatch ip input (xImm,yImm,zImm) opCode = do
  let x = get (ip + 1) input
      y = get (ip + 2) input
      p = get (ip + 3) input
      valX = if xImm then x else get x input
      valY = if yImm then y else get y input
  go (ip + 4) $ set p (valX `opCode` valY) input

get :: Int -> V.Vector Int -> Int
get = flip (V.!)

set :: Int -> Int -> V.Vector Int -> V.Vector Int
set idx val input = input V.// [(idx,val)]

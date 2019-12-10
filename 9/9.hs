{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Control.Monad
import           Control.Monad.IO.Class (liftIO)

import           Data.List.Split

import           Control.Monad.State    (StateT)
import qualified Control.Monad.State    as S
import qualified Data.Map               as M

main :: IO ()
main = do
  input <- map read . splitOn "," <$> readFile "input.txt"
  let largeInput = M.fromList $ zip [0..] input
  void $ flip S.execStateT 0 (go 0 largeInput)

-- | day 5
parseOpCode :: Int -> Int
parseOpCode (read . reverse . take 2 . reverse . show -> x) = x

data Mode = Position | Immediate | Relative
  deriving (Show, Eq)

toMode '0' = Position
toMode '1' = Immediate
toMode '2' = Relative

getParamModes
  :: Int
  -> (Mode, Mode, Mode)
getParamModes x | x < 100 = (Position, Position, Position)
getParamModes (read @Int . reverse . drop 2 . reverse . show -> x) =
  case show x of
    [] -> (Position, Position, Position)
    [x] -> (toMode x,Position, Position)
    [y,x] -> (toMode x, toMode y, Position)
    [z,y,x] -> (toMode x, toMode y, toMode z)

debug :: Bool
debug = False

type RelBase = Int

-- | Part 1
go :: Int -> M.Map Int Int -> StateT RelBase IO ()
go ip input = do
  let inp = get ip input
  when debug . liftIO $ do
    print ("opcode", parseOpCode $ get ip input, getParamModes inp, get ip input)
  case parseOpCode $ get ip input of
    99 -> pure ()
    1  -> dispatch ip input (getParamModes inp) (+)
    2  -> dispatch ip input (getParamModes inp) (*)
    3  -> dispatch3 ip input (getParamModes inp)
    4  -> dispatch4 ip input (getParamModes inp)
    5  -> dispatchJmp ip input (getParamModes inp) (/=)
    6  -> dispatchJmp ip input (getParamModes inp) (==)
    7  -> dispatchCmp ip input (getParamModes inp) (<)
    8  -> dispatchCmp ip input (getParamModes inp) (==)
    9  -> relBaseOffset ip input (getParamModes inp)
    _  -> error "impossible"

relBaseOffset ip input k@(xImm, _, _) = do
  let x = get (ip + 1) input
  relBase <- S.get
  valX <-
   pure $ case xImm of
     Immediate -> x
     Position  -> get x input
     Relative  -> get (relBase + x) input
  S.modify (+valX)
  go (ip + 2) input

-- | dispatchCmp
dispatchCmp ip input (xMode, yMode, zMode) op = do
  let x = get (ip + 1) input
      y = get (ip + 2) input
      p = get (ip + 3) input
  relBase <- S.get
  valX <-
    pure $ case xMode of
      Immediate -> x
      Position -> get x input
      Relative -> get (relBase + x) input
  valY <-
    pure $ case yMode of
      Immediate -> y
      Position -> get y input
      Relative -> get (relBase + y) input
  p' <-
    pure $ case zMode of
      Immediate -> error "impossible"
      Position -> p
      Relative -> relBase + p
  if valX `op` valY
    then go (ip + 4) $ set p' 1 input
    else go (ip + 4) $ set p' 0 input

-- | jmp-if-true
dispatchJmp ip input (xImm, yImm, _) op = do
  let x = get (ip + 1) input
      y = get (ip + 2) input
  relBase <- S.get
  valX <-
    pure $ case xImm of
      Immediate -> x
      Position -> get x input
      Relative -> get (relBase + x) input
  valY <-
    pure $ case yImm of
      Immediate -> y
      Position -> get y input
      Relative -> get (relBase + y) input
  if valX `op` 0
    then go valY input
    else go (ip+3) input

dispatch3 ip input (mode,_,_)= do
  let x = get (ip + 1) input
  relBase <- S.get
  valX <-
    pure $ case mode of
      Immediate -> error "impossible"
      Position  -> x
      Relative  -> x + relBase
  v :: Int <- liftIO (read <$> getLine)
  go (ip + 2) $ set valX v input

dispatch4 ip input (xImm,_,_) = do
  let x = get (ip + 1) input
  relBase <- S.get
  () <-
    liftIO . print $
      case xImm of
        Immediate -> x
        Position -> get x input
        Relative -> get (relBase + x) input
  go (ip + 2) input

dispatch ip input (xImm,yImm,zImm) opCode = do
  let x = get (ip + 1) input
      y = get (ip + 2) input
      p = get (ip + 3) input
  relBase <- S.get
  valX <-
    pure $ case xImm of
      Immediate -> x
      Position -> get x input
      Relative -> get (relBase + x) input
  valY <-
    pure $ case yImm of
      Immediate -> y
      Position -> get y input
      Relative -> get (relBase + y) input
  p' <-
    pure $ case zImm of
      Immediate -> error "impossible"
      Position -> p
      Relative -> relBase + p
  go (ip + 4) $ set p' (valX `opCode` valY) input

get :: Int -> M.Map Int Int -> Int
get key hmap' =
  case M.lookup key hmap' of
    Nothing -> 0
    Just k -> k

set :: Int -> Int -> M.Map Int Int -> M.Map Int Int
set = M.insert

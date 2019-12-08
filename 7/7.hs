{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Control.Lens        hiding (set, get)
import           Data.Function       (on)
import           Data.List           (permutations, sortBy)
import           Data.List.Split     (splitOn)
import qualified Data.Vector         as V

import           Control.Monad.State hiding (get)
import qualified Control.Monad.State as S

data Amps = Amps
  { _amp1 :: (Int, V.Vector Int)
  , _amp2 :: (Int, V.Vector Int)
  , _amp3 :: (Int, V.Vector Int)
  , _amp4 :: (Int, V.Vector Int)
  , _amp5 :: (Int, V.Vector Int)
  }

$(makeLenses ''Amps)

main :: IO ()
main = do
  part1
  part2

-- Part 1. Works great, yay
part1 :: IO ()
part1 = do
  input <- V.fromList . map read . splitOn "," <$> readFile "input.txt"
  xs <- forM (permutations [0..4]) (execute 0 input)
  print $ last $ sortBy (compare `on` snd) xs
    where
      run = flip execStateT
      execute o0 inp p@[a,b,c,d,e]  = do
        [o1] <- run [a,o0] (runIC inp)
        [o2] <- run [b,o1] (runIC inp)
        [o3] <- run [c,o2] (runIC inp)
        [o4] <- run [d,o3] (runIC inp)
        [o5] <- run [e,o4] (runIC inp)
        pure (p,o5)

-- | Part 1
runIC :: V.Vector Int -> StateT [Int] IO (Int, V.Vector Int)
runIC = go 0

mkAmps :: V.Vector Int -> Amps
mkAmps inp = Amps x x x x x
 where
   x = (0,inp)

part2 :: IO ()
part2 = do
  input <- V.fromList . map read . splitOn "," <$> readFile "input.txt"
  let amps = mkAmps input
  xs <- forM (permutations [5..9]) (execute [0] amps)
  print $ last $ sortBy (compare `on` snd) xs
    where
      run = flip runStateT
      execute o0 amps p@[a,b,c,d,e] = flip evalStateT amps $ do
        amp1' <- use amp1
        (s1,[]) <- lift $ run [a] (uncurry go amp1')
        amp1 .= s1
        amp2' <- use amp2
        (s2,[]) <- lift $ run [b] (uncurry go amp2')
        amp2 .= s2
        amp3' <- use amp3
        (s3,[]) <- lift $ run [c] (uncurry go amp3')
        amp3 .= s3
        amp4' <- use amp4
        (s4,[]) <- lift $ run [d] (uncurry go amp4')
        amp4 .= s4
        amp5' <- use amp5
        (s5,[]) <- lift $ run [e] (uncurry go amp5')
        amp5 .= s5
        loop [0] []
          where
            loop [k] [z] | k == z = pure (p, z)
            loop o0 _ = do
              amp1' <- use amp1
              (s1,o1) <- lift $ run o0 (uncurry go amp1')
              amp1 .= s1
              amp2' <- use amp2
              (s2,o2) <- lift $ run o1 (uncurry go amp2')
              amp2 .= s2
              amp3' <- use amp3
              (s3,o3) <- lift $ run o2 (uncurry go amp3')
              amp3 .= s3
              amp4' <- use amp4
              (s4,o4) <- lift $ run o3 (uncurry go amp4')
              amp4 .= s4
              amp5' <- use amp5
              (s5,o5) <- lift $ run o4 (uncurry go amp5')
              amp5 .= s5
              loop o5 o4

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

go :: Int -> V.Vector Int -> StateT [Int] IO (Int, V.Vector Int)
go ip input = do
  let inp = get ip input
  case parseOpCode $ get ip input of
    99 -> pure (ip,input)
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
  xs <- S.get
  case xs of
    [] -> pure (ip, input)
    (x:xs) -> do
      put xs
      let addr = get (ip + 1) input
      go (ip + 2) $ set addr x input

dispatch4 ip input (xImm,_,_) = do
  let addr = get (ip + 1) input
  let val = get addr input
  put [if xImm then addr else val]
  pure (ip + 2, input)

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

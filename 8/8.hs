module Main where

import           Control.Monad
import           Data.Char
import           Data.Function   (on)
import           Data.List
import           Data.List.Split
import qualified Data.Map        as M

type Image = M.Map (Int,Int) Pixel

data Pixel = Black | White | Transparent
  deriving (Show, Eq, Ord)

toPixel 0 = Black
toPixel 1 = White
toPixel 2 = Transparent

main :: IO ()
main = do
  part1
  part2

part1 :: IO ()
part1 = do
  bytes <- readFile "input.txt"
  let nums = map digitToInt (filter isDigit bytes)
      dims = 25 * 6
  let k = head $ sortBy (compare `on` zeroDigits) (chunksOf dims nums)
  print $ length (filter (==1) k) * length (filter (==2) k)

part2 :: IO ()
part2 = do
  bytes <- readFile "input.txt"
  let nums = map digitToInt (filter isDigit bytes)
      dims = 25 * 6
      chunks = chunksOf dims nums
  renderImage (populateImage 6 25 chunks) (6,25)

populateImage :: Int -> Int -> [[Int]] -> Image
populateImage h w xs = foldl' go M.empty xs
  where
    go image xs =
      foldl' goImage
        image (toImageData xs (h,w))

    goImage :: Image -> ((Int,Int),Pixel) -> Image
    goImage img ((k,v),p) = M.insertWith updatePixel (k,v) p img

    updatePixel :: Pixel -> Pixel -> Pixel
    updatePixel _ Black = Black
    updatePixel _ White = White
    updatePixel x Transparent = x

    toImageData :: [Int] -> (Int,Int) -> [((Int,Int),Pixel)]
    toImageData pixels (x,y) = zip (genDims x y) (toPixel <$> pixels)

    genDims
      :: Int
      -> Int
      -> [(Int, Int)]
    genDims x y =
      [ (x',y')
      | x' <- [1..x]
      , y' <- [1..y]
      ]

renderImage :: Image -> (Int,Int) -> IO ()
renderImage hmap (height,width) =
  forM_ [1..height] $ \x -> do
    forM_ [1..width] $ \y -> do
      case M.lookup (x, y) hmap of
        Nothing -> putChar '-'
        Just piece -> putChar (showPixel piece)
    putChar '\n'

showPixel :: Pixel -> Char
showPixel Black = ' '
showPixel White = '%'
showPixel Transparent = '|'

zeroDigits :: [Int] -> Int
zeroDigits = length . filter (==0)


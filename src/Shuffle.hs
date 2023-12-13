module Shuffle
  ( shuffle,
    randomWordGenerator,
  )
where

import System.Random
import Data.Char

shuffle :: [String] -> IO [String]
shuffle x = imp x []

imp :: [String] -> [String] -> IO [String]
imp [] y = return y
imp x y = do
    n <- getStdRandom (randomR (0,(length x) - 1))
    let (a,b) = splitAt n x
    imp (a ++ tail b) ((x!!n):y)

randomWordGenerator :: Int -> IO String
randomWordGenerator 0 = return ""
randomWordGenerator i = do
  n <- getStdRandom (randomR (0, 25))
  l <- randomWordGenerator $ i-1
  return $ (chr $ ord 'a' + n) : l

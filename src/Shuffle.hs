module Shuffle
  ( shuffle,
  )
where

import System.Random

shuffle :: [String] -> IO [String]
shuffle x = imp x []

imp :: [String] -> [String] -> IO [String]
imp [] y = return y
imp x y = do
    n <- randomRIO (0,(length x) - 1)
    let (a,b) = splitAt n x
    imp (a ++ tail b) ((x!!n):y)
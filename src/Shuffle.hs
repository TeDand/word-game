module Shuffle
  ( shuffle,
  )
where

import System.Random

shuffle :: [String] -> [String] -> IO [String]
shuffle [] y = return y
shuffle x y = do
    n <- randomRIO (0,(length x) - 1)
    let (a,b) = splitAt n x
    shuffle (a ++ tail b) ((x!!n):y)
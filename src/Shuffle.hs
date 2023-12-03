module Shuffle
  ( sample,
    shuffle,
    example,
  )
where

import System.Random

sample :: [String]
sample = ["abc","def","ghi","ucd"]

shuffle :: [String] -> [String] -> IO [String]
shuffle [] y = return y
shuffle x y = do
    n <- randomRIO (0,(length x) - 1)
    let (a,b) = splitAt n x
    shuffle (a ++ tail b) ((x!!n):y)

example :: IO ()
example = do 
    list <- shuffle sample []
    print list
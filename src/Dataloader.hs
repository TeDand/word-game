module Dataloader (loadWords, Difficulty (Easy, Hard, Nightmare), lowerString) where

import Shuffle
import System.IO
import System.Random
import Data.Char (toLower)

data Difficulty = Easy | Hard | Nightmare deriving (Show, Eq)

loadWords :: Difficulty -> IO [String]
loadWords diff = case diff of
  Nightmare -> do
    all_words <- generateRandomStrings 20 6
    return (toLowerStrings all_words [])
  _ -> do
    all_words <- loadFile "data/words.txt"
    shuffled <- shuffle $ lines all_words
    return (toLowerStrings shuffled [])

-- Function to generate a random string of a given length
randomString :: Int -> IO String
randomString len = do
  gen <- newStdGen
  return (take len $ randomRs ('a', 'z') gen)

-- Function to generate a list of random strings
generateRandomStrings :: Int -> Int -> IO [String]
generateRandomStrings numStrings stringLength =
  sequence $ replicate numStrings (randomString stringLength)

loadFile :: FilePath -> IO String
loadFile f = do
  handle <- openFile f ReadMode
  hGetContents handle -- this is lazy, you have to do something with it or it will not run


toLowerStrings :: [String] -> [String] -> [String]
toLowerStrings [] acc = acc
toLowerStrings l acc = toLowerStrings (tail l) (acc ++ ([lowerString (head l)]))

lowerString :: String -> String
lowerString s = map toLower s

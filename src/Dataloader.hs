module Dataloader
(loadWords, Difficulty (Easy, Hard, Nightmare)) 
where

import System.IO
import Shuffle

data Difficulty = Easy | Hard | Nightmare deriving (Show, Eq)

loadWords :: Difficulty -> IO [String]
loadWords diff = case diff of
    Nightmare -> do {all_words <- loadFile "data/nightmare_words.txt";
                    shuffle $ lines all_words}
    _ -> do {all_words <- loadFile "data/words.txt";
                    shuffle $ lines all_words}


loadFile :: FilePath -> IO String
loadFile f = do
    handle <- openFile f ReadMode
    contents <- hGetContents handle -- this is lazy, you have to do something with it or it will not run
    return contents
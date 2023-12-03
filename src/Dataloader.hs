module Dataloader
(loadWords, ) 
where

import System.IO

loadWords :: IO [String]
loadWords = do {
    all_words <- loadFile "data/words.txt";
    return (lines all_words)
}


loadFile :: FilePath -> IO String
loadFile f = do
    handle <- openFile f ReadMode
    contents <- hGetContents handle -- this is lazy, you have to do something with it or it will not run
    return contents
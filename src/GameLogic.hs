{-# LANGUAGE FlexibleContexts #-}
module GameLogic 
(gameLoop, )
where

import Text.Printf

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Edit as B
import qualified Brick.Forms as B

gameLoop :: [String] -> Int -> IO String
gameLoop wordList score = 
        do {
            if null wordList
                then 
                    putStrLn (printf "Congratulations you have completed the game! Your score is %d" score);
                else 
                    putStrLn (wordList !! 0);
                    _ <- wordLoop (wordList !! 0);
                    gameLoop (tail wordList) (score + 1);
        }

wordLoop :: String -> IO String
wordLoop wordToMatch = do {
    putStrLn "Input the word!";
    userInput <- getLine;
    if userInput == wordToMatch then do {putStrLn "Correct!\n"; return ""}
    else wordLoop wordToMatch
}


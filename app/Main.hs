module Main (main) where

import Tui
import Scoreboard


main :: IO ()
main = do {
    putStrLn "Press Enter to Start the Game";
    _ <- getLine;
    finalScore <- tui;
    scoreBoard <- readScoreboard;
    putStrLn "Enter your username to save your score!";
    username <- getLine;
    
  
    writtenBoard <- writeScoreboard (addNewScore scoreBoard username finalScore);
    putStrLn "Current Scoreboard:";
    print (writtenBoard)
}
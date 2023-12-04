module Main (main) where

import Scoreboard
import Tui

-- main :: IO Int
-- main = do
--   putStrLn "Press Enter to Start the Game"
--   _ <- getLine
--   finalScore <- tui
--   scoreBoard <- readScoreboard
--   putStrLn "Enter your username to save your score!"
--   username <- getLine

--   writtenBoard <- writeScoreboard (addNewScore scoreBoard username finalScore)
--   putStrLn "Current Scoreboard:"
--   print (writtenBoard)

main :: IO ()
main = tui
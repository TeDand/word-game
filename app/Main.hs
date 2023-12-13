module Main (main) where

import Menu

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
main = menu 1
    -- do
    -- w <- loadWords Hard
    -- w <- (return ["abc","def","xyz"])
    -- sw <- shuffle w
    -- print(sw)

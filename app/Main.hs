module Main (main) where

import Dataloader (Difficulty (Easy, Hard, Nightmare))
import Scoreboard
import Tui (tui)

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
-- main = tui
main = do
  putStrLn "Press Enter to Start the Game"
  _ <- getLine
  putStrLn "Select your difficulty level! Easy, Hard, Nightmare" -- Should be in a menu
  userInput <- getLine
  difficulty <- convertDifficulty userInput

  finalScore <- tui difficulty -- Wtf how do I pass an argument in
  scoreBoard <- readScoreboard
  putStrLn "Enter your username to save your score!"
  username <- getLine

  writtenBoard <- writeScoreboard (addNewScore scoreBoard username finalScore)
  putStrLn "Current Scoreboard:"
  print writtenBoard

-- convertDifficulty is a placeholder and the menu should feed the real difficulty level in
convertDifficulty :: String -> IO Difficulty
convertDifficulty diff = case diff of
  "Easy" -> return Easy
  "Hard" -> return Hard
  "Nightmare" -> return Nightmare
  _ -> return Easy

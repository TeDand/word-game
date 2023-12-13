module Tui (tui) where

import Attributes
import Brick
import Brick.BChan
import Control.Concurrent
import Control.Monad
import Dataloader (Difficulty (Easy, Hard, Nightmare), loadWords)
import EventHandler
import GameState
import qualified Graphics.Vty as V
import RenderState

tui :: Difficulty -> IO Int
tui diff = do
  chan <- newBChan 10

  void $ forkIO $ forever $ do
    writeBChan chan MoveRight
    threadDelay 100000 -- enemy
  void $ forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 1000000 -- game timer
  initialState <- buildInitialState diff
  (endState, vty) <- customMainWithDefaultVty (Just chan) tuiApp initialState
  V.shutdown vty
  return (currentScore endState)

-- return $ currentScore endState

tuiApp :: App TuiState CustomEvent ResourceName
tuiApp =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent,
      appStartEvent = return (),
      appAttrMap = gameMap
    }

buildInitialState :: Difficulty -> IO TuiState
buildInitialState difficulty = case difficulty of
  Easy -> buildEasyInitialState
  Hard -> buildHardInitialState
  Nightmare -> buildNightmareInitialState

buildEasyInitialState :: IO TuiState
buildEasyInitialState = do
  -- Easy: Just one word
    wordsToType <- loadWords Easy-- Add difficulty of words later
    return
      TuiState
        { tuiStateTarget = [head wordsToType],
          tuiStateInput = "",
          remainingWords = tail wordsToType ++ [""],
          currentScore = 0,
          timer = 0,
          distance = [0],
          health = 1.0,
          announcement = (0, ""),
          difficultyLevel = Easy
        }

buildHardInitialState :: IO TuiState
buildHardInitialState = do
  -- Hard: 3 different words. Need to type all 3 to destroy all 3.
  -- But the words should be reasonably short
    wordsToType <- loadWords Hard
    return
      TuiState
        { tuiStateTarget = [head wordsToType, wordsToType !! 1, wordsToType !! 2],
          tuiStateInput = "",
          remainingWords = drop 3 wordsToType ++ ["","",""],
          currentScore = 0,
          timer = 0,
          distance = [0,0,0],
          health = 1.0,
          announcement = (0, ""),
          difficultyLevel = Hard
        }

buildNightmareInitialState :: IO TuiState
buildNightmareInitialState = do
  -- Nightmare: 3 different words. Need to type all 3 to destroy all 3.
  -- The words are ridiculous and nonsensical compared to hard
  -- Might also want to lower the timer
    wordsToType <- loadWords Nightmare
    return
      TuiState
        { tuiStateTarget = [head wordsToType, wordsToType !! 1, wordsToType !! 2],
          tuiStateInput = "",
          remainingWords = drop 3 wordsToType ++ ["","",""],
          currentScore = 0,
          timer = 0,
          distance = [0,0,0],
          health = 1.0,
          announcement = (0, ""),
          difficultyLevel = Nightmare
        }

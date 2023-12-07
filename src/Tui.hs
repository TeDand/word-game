module Tui (tui) where

import Attributes
import Brick
import Brick.BChan
import Control.Concurrent
import Control.Monad
import Dataloader (loadWords)
import EventHandler
import GameState
import RenderState

tui :: IO ()
tui = do
  chan <- newBChan 10

  void $ forkIO $ forever $ do
    writeBChan chan MoveRight
    threadDelay 100000 -- enemy
  void $ forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 1000000 -- game timer
  initialState <- buildInitialState
  void $ customMainWithDefaultVty (Just chan) tuiApp initialState
  endState <- defaultMain tuiApp initialState
  print endState

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

buildInitialState :: IO TuiState
buildInitialState = do
  wordsToType <- loadWords
  return
    TuiState
      { tuiStateTarget = head wordsToType,
        tuiStateInput = "",
        remainingWords = tail wordsToType,
        currentScore = 0,
        timer = 20,
        distance = 0,
        level = 1,
        health = 1.0,
        announcement = (0, "")
      }

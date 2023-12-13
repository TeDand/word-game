module GameState (TuiState (..), ResourceName) where

import Dataloader (Difficulty)

data TuiState = TuiState
  { tuiStateTarget :: [String],
    tuiStateInput :: String,
    currentScore :: Int,
    remainingWords :: [String],
    timer :: Int,
    distance :: [Int],
    health :: Float,
    difficultyLevel :: Difficulty,
    announcement :: (Int, String)
  }
  deriving (Show, Eq)

type ResourceName = String
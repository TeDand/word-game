module GameState (TuiState (..), ResourceName) where

data TuiState = TuiState
  { tuiStateTarget :: String,
    tuiStateInput :: String,
    currentScore :: Int,
    remainingWords :: [String],
    timer :: Int,
    distance :: Int,
    level :: Int,
    health :: Float,
    announcement :: (Int, String)
  }
  deriving (Show, Eq)

type ResourceName = String
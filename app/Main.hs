module Main (main) where

import Brick
-- import Brick

import qualified Brick as B
import qualified Brick.Forms as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import Brick.Widgets.Core
import qualified Brick.Widgets.Edit as B
import Control.Monad.State
import Dataloader
import Game
import Graphics.Vty
import Scoreboard
import Ships
import Tui


main :: IO ()
{-
main = do
  putStrLn "Press Enter to start"
  _ <- getLine
  words <- loadWords
  let app = gameApp
      initialState = GameState {currentWord = "", currentScore = 0, started = True, inputSoFar = "", allPossibleWords = words}
  finalState <- B.defaultMain app initialState
  return ()
-}
-- main = someFunc
main = game
-- main = expScoreboard
-- main = tui

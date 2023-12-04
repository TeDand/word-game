module Main (main) where

import Brick
import qualified Brick as B
import qualified Brick.Forms as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import Brick.Widgets.Core
import qualified Brick.Widgets.Edit as B
import Control.Monad.State
import Dataloader
-- import Game
import Graphics.Vty
import Scoreboard
import Ships
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
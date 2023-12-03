module Main (main) where

import Lib

import Hello

import Dataloader

import GameLogic (gameLoop)

import Control.Monad.State

-- import Brick

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import qualified Brick.Widgets.Edit as B
import qualified Brick.Forms as B

import Graphics.Vty


{-
main :: IO String
main = do {
            putStrLn "Press any key to start";
            _ <- getLine;
            words <- loadWords;
            gameLoop words 0 ;
        }

-- https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst
data App s e n = {
    App {
        appDraw         :: s -> [Widget n]
        , appChooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
        , appHandleEvent  :: BrickEvent n e -> EventM n s ()
        , appStartEvent   :: EventM n s ()
        , appAttrMap      :: s -> AttrMap
    }
}
-}

data GameEvent = GameEvent

data GameState = GameState { currentWord :: String
                            ,currentScore :: Integer
                            ,started :: Bool
                            ,inputSoFar :: String
                            ,allPossibleWords :: [String]
}

type ResourceName = ()  -- No idea what this is yet

drawGame :: GameState -> [B.Widget ResourceName]
drawGame gs = case (started gs) of
    False -> [B.str "Press start to enter a new game"] -- B.center $ B.str "Press start to enter a new game"
    True ->  [B.str (currentWord gs)] -- B.center $ B.str (currentWord gs)

gameStartEvent :: B.EventM n s ()
gameStartEvent = return ()


gameApp :: B.App GameState GameEvent ResourceName
gameApp = B.App {B.appDraw = drawGame
,B.appChooseCursor = B.neverShowCursor
,B.appHandleEvent = handleGameEvent
,B.appStartEvent = gameStartEvent
,B.appAttrMap = ()
}


-- For handling events --

trackUserInput :: GameState -> GameState
trackUserInput currentState newUserInput = GameState {
    currentWord = currentWord currentState, 
    currentScore = currentScore currentState,
    started = started currentState, 
    inputSoFar = (inputSoFar currentState) ++ newUserInput,
    allPossibleWords = allPossibleWords currentState
}

verifyInputAgainstWord :: GameState -> GameState
verifyInputAgainstWord currentState = if (currentWord currentState) == (currentScore currentState) 
    then
        -- User has input the words correctly
        GameState {
            currentWord = head (allPossibleWords currentState),
            currentScore = (currentScore currentState) + 1,
            started = (started currentState), 
            inputSoFar = "",
            allPossibleWords = tail (allPossibleWords currentState)
        }
    else
        GameState {
            currentWord = currentWord currentState,
            currentScore = currentScore currentState,
            started = started currentState, 
            inputSoFar = "", -- Reset the word for the user to try again
            allPossibleWords = allPossibleWords currentState

        }

handleGameEvent :: GameState -> (B.BrickEvent ResourceName GameEvent) -> B.EventM ResourceName GameState ()
handleGameEvent state (B.VtyEvent (KChar inputChar)) = return $ trackUserInput state inputChar
handleGameEvent state (B.VtyEvent (KEnter)) = return $ verifyInputAgainstWord state



main :: IO ()
main = do 
    let 
        app = B.App {B.appDraw = drawGame}
        initialState = GameState {currentWord = "", currentScore = 0, started = False}
    finalState <- B.defaultMain app initialState
    return ()

module Main (main) where

import Scoreboard

import Game
import Ship
import Brick
import Brick.Widgets.Core

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

type ResourceName = ()  -- No idea what this is for yet tbh

drawGame :: GameState -> [B.Widget ResourceName]
drawGame gs = case (allPossibleWords gs) of 
    x:xs -> [(B.str ("Type this:") B.<+> B.str (currentWord gs)) B.<=> (B.str (inputSoFar gs)) B.<=> (B.str ("Current Score:") B.<+> B.str(show $ currentScore gs))] -- B.center $ B.str (currentWord gs)
    [] -> [(B.str ("You have beaten the game! Your final score is:") B.<+> B.str(show $ currentScore gs))]

gameStartEvent :: B.EventM n s ()
gameStartEvent = return () -- Docs say for most programs, this will just be a return


gameApp :: B.App GameState GameEvent ResourceName
gameApp = B.App {
    B.appDraw = drawGame,
    B.appChooseCursor = B.neverShowCursor,
    B.appHandleEvent = handleGameEvent,
    B.appStartEvent = gameStartEvent,
    B.appAttrMap = attrMap
}


attrMap :: GameState -> B.AttrMap
attrMap s = B.attrMap (B.bg white) []

-- For handling events --

trackUserInput :: Char -> B.EventM ResourceName GameState ()
trackUserInput newUserInput = do {
    currentState <- get;
    put (GameState {
        currentWord = currentWord currentState, 
        currentScore = currentScore currentState,
        inputSoFar = (inputSoFar currentState) ++ [newUserInput],
        allPossibleWords = allPossibleWords currentState})
}

verifyInputAgainstWord :: B.EventM ResourceName GameState ()
verifyInputAgainstWord = do {
    currentState <- get;
    if (currentWord currentState) == (inputSoFar currentState) 
    then
        -- User has input the words correctly
        put (GameState {
            currentWord = head (allPossibleWords currentState),
            currentScore = (currentScore currentState) + 1,
            inputSoFar = "",
            allPossibleWords = tail (allPossibleWords currentState)
        });
    else
        put (GameState {
            currentWord = currentWord currentState,
            currentScore = currentScore currentState,
            started = started currentState, 
            inputSoFar = "", -- Reset the word for the user to try again
            allPossibleWords = allPossibleWords currentState
        });
}

handleGameEvent :: B.BrickEvent ResourceName GameEvent -> B.EventM ResourceName GameState ()
handleGameEvent (B.VtyEvent (EvKey (KChar inputChar) [])) = trackUserInput inputChar
handleGameEvent (B.VtyEvent (EvKey KEnter [])) = verifyInputAgainstWord



main :: IO ()
main = do
    putStrLn "Press Enter to start";
    _ <- getLine;
    words <- loadWords;
    let 
        app = gameApp
        initialState = GameState {currentWord = "", currentScore = 0, started = True, inputSoFar = "", allPossibleWords = words}
    finalState <- B.defaultMain app initialState
    return ()
    -- main = someFunc
    -- main = game
    -- main = expScoreboard

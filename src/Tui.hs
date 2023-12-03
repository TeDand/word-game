module Tui (tui) where

import Brick
-- import Brick.AttrMap
-- import Brick.Main
-- import Brick.Types
-- import Brick.Widgets.Core

import Brick.Widgets.Center (hCenter)
-- import Cursor.Simple.List.NonEmpty
-- import qualified Data.List.NonEmpty as NE
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

-- import System.Directory
-- import System.Exit

import Dataloader(loadWords)

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState

data TuiState = TuiState
  { tuiStateTarget :: String,
    tuiStateInput :: String,
    currentScore:: Int,
    allPossibleWords :: [String]
  }
  deriving (Show, Eq)

type ResourceName = String

tuiApp :: App TuiState e ResourceName
tuiApp =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent,
      appStartEvent = return (),
      appAttrMap = gameMap
    }

gameMap :: GameState -> B.AttrMap
gameMap s = const $ attrMap defAttr [(attrName "input", fg yellow)]


buildInitialState :: IO TuiState
buildInitialState = do
  let words = loadWords
  return TuiState {
    tuiStateTarget = head words, 
    tuiStateInput = "",
    remainingWords = tail words,
    allPossibleWords}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
  [ vBox
      [ hCenter (str "Type the word: "),
        hCenter (withAttr (attrName "input") $ str inputWord),
        hCenter (str (" (" ++ targetWord ++ ")"))
      ]
  ]
  where
    inputWord = if tuiStateInput ts == "" then " " else tuiStateInput ts
    targetWord = tuiStateTarget ts

handleTuiEvent :: BrickEvent n e -> EventM n TuiState ()
handleTuiEvent e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar c) [] -> do
      modify $ \s -> s {tuiStateInput = tuiStateInput s ++ [c]}
      return ()
    EvKey KBS [] -> do
      modify $ \s -> s {tuiStateInput = init (tuiStateInput s)}
      return ()
    EvKey KEnter [] -> do
      ts <- get
      if tuiStateInput ts == tuiStateTarget ts
        then halt
        else return ()
    EvKey KEsc [] -> halt
    _ -> return ()
  _ -> return ()



-- My shit

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
    B.appAttrMap = gameMap
}




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
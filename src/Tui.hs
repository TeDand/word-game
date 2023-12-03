module Tui (tui) where

import Brick

import Brick.Widgets.Center (hCenter)
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events


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
    remainingWords :: [String]
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

gameMap :: TuiState -> AttrMap
gameMap _ = attrMap defAttr [(attrName "input", fg yellow)]


buildInitialState :: IO TuiState
buildInitialState = do
  wordsToType <- loadWords
  return TuiState {
    tuiStateTarget = head wordsToType, 
    tuiStateInput = "",
    remainingWords = tail wordsToType,
    currentScore = 0}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = case remainingWords ts of 
  _:_ -> renderOngoingGameState ts
  [] -> renderGameEndState ts

renderOngoingGameState :: TuiState -> [Widget ResourceName]
renderOngoingGameState ts = [ vBox
      [ hCenter (str "Type the word: "),
        hCenter (withAttr (attrName "input") $ str inputWord),
        hCenter (str (" (" ++ targetWord ++ ")")),
        hCenter (str "CurrentScore: "),
        hCenter (str (show $ currentScore ts))
      ]
  ]
  where
    inputWord = if tuiStateInput ts == "" then " " else tuiStateInput ts
    targetWord = tuiStateTarget ts

renderGameEndState :: TuiState -> [Widget ResourceName]
renderGameEndState ts = [str "You have beaten the game! Your final score is:" <+> str(show $ currentScore ts)]

-- Functions to handle events

handleTuiEvent :: BrickEvent n e -> EventM n TuiState ()
handleTuiEvent e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar c) [] -> addUserInput c
    EvKey KBS [] -> removeUserInput
    EvKey KEnter [] -> verifyInputAgainstWord
    EvKey KEsc [] -> halt
    _ -> return ()
  _ -> return ()

addUserInput :: Char -> EventM n TuiState ()
addUserInput c = do {
    _ <- get;
    modify $ \s -> s {tuiStateInput = tuiStateInput s ++ [c]}
}

removeUserInput :: EventM n TuiState ()
removeUserInput = do {
  _ <- get;
  modify $ \s -> s {tuiStateInput = init (tuiStateInput s)}
}

verifyInputAgainstWord :: EventM n TuiState ()
verifyInputAgainstWord = do {
    currentState <- get;
    if tuiStateTarget currentState == tuiStateInput currentState
    then
        -- User has input the words correctly
        put (TuiState {
            tuiStateTarget = head (remainingWords currentState),
            tuiStateInput = "",
            currentScore = currentScore currentState+ 1,
            remainingWords = tail (remainingWords currentState)
        });
    else
      -- Incorrect; reset the input tracker
        modify $ \s -> s {tuiStateInput = ""}
}

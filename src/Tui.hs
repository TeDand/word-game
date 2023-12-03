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

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState

data TuiState = TuiState
  { tuiStateTarget :: String,
    tuiStateInput :: String
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
      appAttrMap = const $ attrMap defAttr [(attrName "input", fg yellow)]
    }

buildInitialState :: IO TuiState
buildInitialState = do
  let word = "Hello" -- Change this to the word you want to display
  return TuiState {tuiStateTarget = word, tuiStateInput = ""}

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

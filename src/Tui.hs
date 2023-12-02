module Tui (tui) where

import Brick
-- import Brick.AttrMap
-- import Brick.Main
-- import Brick.Types
-- import Brick.Widgets.Core
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import System.Directory

tui :: IO ()
tui = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState

data TuiState = TuiState
  { tiuStatePaths :: [FilePath]
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
      appAttrMap = const $ attrMap defAttr []
    }

buildInitialState :: IO TuiState
buildInitialState = do
  here <- getCurrentDirectory
  contents <- listDirectory here
  pure TuiState {tiuStatePaths = contents}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [vBox $ map drawPath $ tiuStatePaths ts]

drawPath :: FilePath -> Widget n
drawPath = str

handleTuiEvent :: BrickEvent n e -> EventM n TuiState ()
handleTuiEvent e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'q') [] -> halt
    _ -> return ()
  _ -> return ()

module Directory (directory) where

import Brick
-- import Brick.AttrMap
-- import Brick.Main
-- import Brick.Types
-- import Brick.Widgets.Core

import Cursor.Simple.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import System.Directory
import System.Exit

directory :: IO ()
directory = do
  initialState <- buildInitialState
  endState <- defaultMain tuiApp initialState
  print endState

data TuiState = TuiState
  { tuiStatePaths :: NonEmptyCursor FilePath
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
      appAttrMap = const $ attrMap defAttr [(attrName "selected", fg red)]
    }

buildInitialState :: IO TuiState
buildInitialState = do
  here <- getCurrentDirectory
  contents <- listDirectory here
  case NE.nonEmpty contents of
    Nothing -> die "Empty directory"
    Just ne ->
      pure TuiState {tuiStatePaths = makeNonEmptyCursor ne}

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
  [ vBox $
      concat
        [ map (drawPath False) $ reverse $ nonEmptyCursorPrev nec,
          [drawPath True $ nonEmptyCursorCurrent nec],
          map (drawPath False) $ nonEmptyCursorNext nec
        ]
  ]
  where
    nec = tuiStatePaths ts

-- [vBox $ map drawPath $ tuiStatePaths ts]

drawPath :: Bool -> FilePath -> Widget n
drawPath b = (if b then withAttr (attrName "selected") else id) . str

handleTuiEvent :: BrickEvent n e -> EventM n TuiState ()
handleTuiEvent e = case e of
  VtyEvent vtye -> case vtye of
    EvKey (KChar 'q') [] -> halt
    EvKey KDown [] -> do
      ts <- get
      let nec = tuiStatePaths ts
      case nonEmptyCursorSelectNext nec of
        Just nec' -> modify $ \s -> s {tuiStatePaths = nec'}
        Nothing -> return ()
    EvKey KUp [] -> do
      ts <- get
      let nec = tuiStatePaths ts
      case nonEmptyCursorSelectPrev nec of
        Just nec' -> modify $ \s -> s {tuiStatePaths = nec'}
        Nothing -> return ()
    _ -> return ()
  _ -> return ()

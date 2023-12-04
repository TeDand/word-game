module Tui (tui) where

import Brick
import Brick.BChan
import qualified Brick.Widgets.ProgressBar as P
import Control.Concurrent
import Control.Monad
import Dataloader (loadWords)
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Ships

data CustomEvent = MoveRight | Tick deriving (Show)

tui :: IO ()
tui = do
  chan <- newBChan 10

  void $ forkIO $ forever $ do
    writeBChan chan MoveRight
    threadDelay 100000 -- enemy
  void $ forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 1000000 -- game timer
  initialState <- buildInitialState
  void $ customMainWithDefaultVty (Just chan) tuiApp initialState
  endState <- defaultMain tuiApp initialState
  print endState

-- return $ currentScore endState

data TuiState = TuiState
  { tuiStateTarget :: String,
    tuiStateInput :: String,
    currentScore :: Int,
    remainingWords :: [String],
    timer :: Int,
    distance :: Int,
    level :: Int,
    health :: Float
  }
  deriving (Show, Eq)

type ResourceName = String

tuiApp :: App TuiState CustomEvent ResourceName
tuiApp =
  App
    { appDraw = drawTui,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleTuiEvent,
      appStartEvent = return (),
      appAttrMap = gameMap
    }

gameMap :: TuiState -> AttrMap
gameMap _ =
  attrMap
    defAttr
    [ (attrName "input", fg yellow),
      (theBaseAttr, bg brightBlack),
      (healthDoneAttr, blue `on` green),
      (healthToDoAttr, blue `on` red),
      (P.progressIncompleteAttr, fg yellow)
    ]

theBaseAttr :: AttrName
theBaseAttr = attrName "theBase"

healthDoneAttr, healthToDoAttr :: AttrName
healthDoneAttr = theBaseAttr <> attrName "health:done"
healthToDoAttr = theBaseAttr <> attrName "health:remaining"

buildInitialState :: IO TuiState
buildInitialState = do
  wordsToType <- loadWords
  return
    TuiState
      { tuiStateTarget = head wordsToType,
        tuiStateInput = "",
        remainingWords = tail wordsToType,
        currentScore = 0,
        timer = 20,
        distance = 0,
        level = 1,
        health = 1.0
      }

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = case remainingWords ts of
  _ : _ -> renderOngoingGameState ts
  [] -> renderGameEndState ts

renderOngoingGameState :: TuiState -> [Widget ResourceName]
renderOngoingGameState ts = [a]
  where
    -- vBox
    --     [ hCenter (str "Type the word: "),
    --       hCenter (withAttr (attrName "input") $ str inputWord),
    --       hCenter (str (" (" ++ targetWord ++ ")")),
    --       hCenter (str "CurrentScore: "),
    --       hCenter (str (show $ currentScore ts)),
    --     ]

    a =
      (str $ "Level: " <> (show $ (level ts)))
        <=> (str $ "Time Left: " <> (show $ (timer ts)))
        <=> enemyShip ts targetWord
        <=> str inputWord
        <=> padTop (Pad 5) (getHealth ts)
    inputWord = if tuiStateInput ts == "" then " " else tuiStateInput ts
    targetWord = tuiStateTarget ts

getHealth :: TuiState -> Widget ResourceName
getHealth st = ui
  where
    healthBar =
      updateAttrMap
        ( mapAttrNames
            [ (healthDoneAttr, P.progressCompleteAttr),
              (healthToDoAttr, P.progressIncompleteAttr)
            ]
        )
        $ bar
        $ health st
    lbl c = Just $ show $ fromEnum $ c * 100
    bar v = P.progressBar (lbl v) v
    ui =
      (str "Health: " <+> healthBar)

enemyShip :: TuiState -> String -> Widget ResourceName
enemyShip ts word =
  padTop
    (Pad 2)
    ( evilShip
        <+> (str $ replicate ((distance ts) - 1) ' ' <> word)
        <+> ship
    )

renderGameEndState :: TuiState -> [Widget ResourceName]
renderGameEndState ts = [str "You have beaten the game! Your final score is:" <+> str (show $ currentScore ts)]

-- Functions to handle events

handleTuiEvent :: BrickEvent n CustomEvent -> EventM n TuiState ()
handleTuiEvent e = case e of
  AppEvent MoveRight -> do
    ts <- get
    let d = distance ts
    modify $ \s -> s {distance = if d < 90 then d + 1 else 0}
  -- distance %= (\row -> if row < 90 then row + 1 else 0)
  AppEvent Tick -> do
    ts <- get
    let t = timer ts
    modify $ \s -> s {timer = if t > 0 then t - 1 else 0}
  -- timer %= (\c -> if c > 0 then c - 1 else 0)
  VtyEvent vtye -> case vtye of
    EvKey (KChar c) [] -> addUserInput c
    EvKey KBS [] -> removeUserInput
    EvKey KEnter [] -> verifyInputAgainstWord
    EvKey KEsc [] -> halt
    _ -> return ()
  _ -> return ()

addUserInput :: Char -> EventM n TuiState ()
addUserInput c = do
  _ <- get
  modify $ \s -> s {tuiStateInput = tuiStateInput s ++ [c]}

removeUserInput :: EventM n TuiState ()
removeUserInput = do
  _ <- get
  modify $ \s -> s {tuiStateInput = init (tuiStateInput s)}

verifyInputAgainstWord :: EventM n TuiState ()
verifyInputAgainstWord = do
  currentState <- get
  if tuiStateTarget currentState == tuiStateInput currentState
    then -- User has input the words correctly

      put
        ( TuiState
            { tuiStateTarget = head (remainingWords currentState),
              tuiStateInput = "",
              currentScore = currentScore currentState + 1,
              remainingWords = tail (remainingWords currentState),
              timer = timer currentState,
              distance = distance currentState,
              level = level currentState,
              health = health currentState
            }
        )
    else -- Incorrect; reset the input tracker
    modify $ \s -> s {tuiStateInput = ""}

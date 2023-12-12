module EventHandler (handleTuiEvent, CustomEvent (MoveRight, Tick)) where

import Brick
import GameState
import Graphics.Vty.Input.Events
import Control.Monad

data CustomEvent = MoveRight | Tick deriving (Show)

handleTuiEvent :: BrickEvent n CustomEvent -> EventM n TuiState ()
handleTuiEvent e = case e of
  AppEvent MoveRight -> do
    ts <- get
    let d = distance ts
    modify $ \s -> s {distance = if d < 90 then d + 1 else 0}
    when (d == 90) $ do
      takeDamage
    when (health ts <= 0) $ do
      halt
  AppEvent Tick -> do
    ts <- get
    let t = timer ts
    modify $ \s -> s {timer = if t > 0 then t - 1 else 0}
    when (t == 0) $ do
      
      increaseLevel 
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
              distance = 0,
              level = level currentState,
              health = health currentState
            }
        )
    else -- Incorrect; reset the input tracker
    modify $ \s -> s {tuiStateInput = ""}

takeDamage :: EventM n TuiState ()
takeDamage = do
  currentState <- get
  put
    ( TuiState
        { tuiStateTarget = head (remainingWords currentState),
          tuiStateInput = tuiStateInput currentState,
          currentScore = currentScore currentState,
          remainingWords = tail (remainingWords currentState),
          timer = timer currentState,
          distance = 0,
          level = level currentState,
          health = health currentState - 0.1
        }
    )
increaseLevel :: EventM n TuiState () -- increases level and resets timer 
increaseLevel = do
  currentState <- get
  put
    ( TuiState
        { tuiStateTarget = tuiStateTarget currentState,
          tuiStateInput = "",
          currentScore = currentScore currentState,
          remainingWords = remainingWords currentState,
          timer = 30,
          distance = 0,
          level = level currentState + 1,
          health = 1.0
        }
    )
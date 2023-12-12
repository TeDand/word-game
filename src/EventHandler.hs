module EventHandler (handleTuiEvent, CustomEvent (MoveRight, Tick)) where

import Brick
import GameState
import Graphics.Vty.Input.Events

data CustomEvent = MoveRight | Tick deriving (Show)

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
    let a = fst (announcement ts)
    modify $ \s -> s {announcement = if a > 0 then (a - 1, snd (announcement ts)) else (0, "")}
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
              health = health currentState,
              announcement = (0, "")
            }
        )
    else do
      -- User has input the words incorrectly
      modify $ \s -> s {tuiStateInput = ""}
      modify $ \s -> s {announcement = (2, "INCORRECT INPUT!")}
      modify $ \s -> s {health = health s - 0.2}
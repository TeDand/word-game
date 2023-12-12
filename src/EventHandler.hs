module EventHandler (handleTuiEvent, CustomEvent (MoveRight, Tick)) where

import Brick
import Dataloader (Difficulty (Easy))
import GameState
import Graphics.Vty.Input.Events
import Control.Monad

data CustomEvent = MoveRight | Tick deriving (Show)

handleTuiEvent :: BrickEvent n CustomEvent -> EventM n TuiState ()
handleTuiEvent e = case e of
  AppEvent MoveRight -> changeDistance
  -- distance %= (\row -> if row < 90 then row + 1 else 0)
  AppEvent Tick -> timerTick
  -- timer %= (\c -> if c > 0 then c - 1 else 0)
  VtyEvent vtye -> case vtye of
    EvKey (KChar c) [] -> addUserInput c
    EvKey KBS [] -> removeUserInput
    EvKey KEnter [] -> verifyInputAgainstWord
    EvKey KEsc [] -> halt
    _ -> return ()
  _ -> return ()

changeDistance :: EventM n TuiState ()
changeDistance = do
  ts <- get
  let d = distance ts
  modify $ \s -> s {distance = if d < 90 then d + 1 else 0}
  when (d == 90) $ do
      takeDamage
    when (health ts <= 0) $ do
      halt

timerTick :: EventM n TuiState ()
timerTick = do
  ts <- get
  let t = timer ts
  modify $ \s -> s {timer = if t > 0 then t - 1 else 0}
  let a = fst (announcement ts)
  modify $ \s -> s {announcement = if a > 0 then (a - 1, snd (announcement ts)) else (0, "")}
  when (t == 0) $ do
      increaseLevel 

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
  case difficultyLevel currentState of
    Easy -> verifyEasyInput
    _ -> verifyNotEasyInput

verifyEasyInput :: EventM n TuiState ()
-- For easy mode, we just compare to the first word, since the word is replicated 3 times
verifyEasyInput = do
  currentState <- get
  if head (tuiStateTarget currentState) == tuiStateInput currentState
    then -- User has input the words correctly

      put
        ( TuiState
            { tuiStateTarget = [head (remainingWords currentState)],
              tuiStateInput = "",
              currentScore = currentScore currentState + 1,
              remainingWords = tail (remainingWords currentState),
              timer = timer currentState,
              distance = 0,
              level = level currentState,
              health = health currentState,
              announcement = (0, ""),
              difficultyLevel = difficultyLevel currentState
            }
        )
    else do
      -- Incorrect; reset the input tracker
      modify $ \s -> s {tuiStateInput = ""}
      modify $ \s -> s {announcement = (2, "INCORRECT INPUT!")}
      takeDamage

verifyNotEasyInput :: EventM n TuiState ()
-- For easy mode, we just compare to the first word
verifyNotEasyInput = do
  currentState <- get
  if head (tuiStateTarget currentState) == tuiStateInput currentState
    then -- User has input the words correctly

      if length (tuiStateTarget currentState) > 1
        then -- there are more words to type in this volley. remove one word,
        modify $ \s ->
          s
            { tuiStateTarget = tail (tuiStateTarget s),
              tuiStateInput = ""
            }
        else -- there are no more words to type in this volley. Go to the next volley!

          put
            ( TuiState
                { tuiStateTarget = getNextVolleyOfWords (remainingWords currentState),
                  tuiStateInput = "",
                  currentScore = currentScore currentState + 1,
                  remainingWords = drop 3 (remainingWords currentState),
                  timer = timer currentState,
                  distance = 90,
                  level = level currentState,
                  health = health currentState,
                  announcement = (0, ""),
                  difficultyLevel = difficultyLevel currentState
                }
            )
    else do
      -- Incorrect; reset the input tracker
      modify $ \s -> s {tuiStateInput = ""}
      modify $ \s -> s {announcement = (2, "INCORRECT INPUT!")}
      takeDamage

getNextVolleyOfWords :: [String] -> [String]
getNextVolleyOfWords wordsList = [head wordsList, wordsList !! 1, wordsList !! 2]

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

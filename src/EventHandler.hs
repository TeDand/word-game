module EventHandler (handleTuiEvent, CustomEvent (MoveRight, Tick)) where

import Brick
import Control.Monad
import Dataloader (Difficulty (Easy))
import GameState
import Graphics.Vty.Input.Events
import qualified Data.List as L

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
  modify $ \s -> s {distance = map (\dist -> if dist < 90 then dist + 1 else 0) d}
  let hit = map (\dist -> if dist == 90 then True else False) (distance ts)
  takeDamage hit
  when (health ts <= 0) $ do
    halt
timerTick :: EventM n TuiState () 
timerTick = do
  ts <- get
  let t = timer ts
  modify $ \s -> s {timer = t+ 1}
  let a = fst (announcement ts)
  modify $ \s -> s {announcement = if a > 0 then (a - 1, snd (announcement ts)) else (0, "")}

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
              distance = [0],
              health = health currentState,
              announcement = (0, ""),
              difficultyLevel = difficultyLevel currentState
            }
        )
    else do
      -- Incorrect; reset the input tracker
      modify $ \s -> s {tuiStateInput = ""}
      modify $ \s -> s {announcement = (2, "INCORRECT INPUT!")}
      takeDamage []

verifyNotEasyInput :: EventM n TuiState ()
-- For easy mode, we just compare to the first word
verifyNotEasyInput = do
  currentState <- get
  if (elem (tuiStateInput currentState) (tuiStateTarget currentState))
    then do
      let spot = case L.elemIndex (tuiStateInput currentState) (tuiStateTarget currentState) of
                  Just num -> num
                  Nothing  -> -1

      put
        ( TuiState
            { tuiStateTarget = replaceWord spot (tuiStateTarget currentState) (head (remainingWords currentState)),
              tuiStateInput = "",
              currentScore = currentScore currentState + 1,
              remainingWords = tail (remainingWords currentState),
              timer = timer currentState,
              distance = replaceDistance spot (distance currentState),
              health = health currentState,
              announcement = (0, ""),
              difficultyLevel = difficultyLevel currentState
            }
        )
    else do
      modify $ \s -> s {tuiStateInput = ""}
      modify $ \s -> s {announcement = (2, "INCORRECT INPUT!")}
      takeDamage []

replaceWord :: Int -> [String] -> String -> [String]
replaceWord index list newWord
  | index < 0 || index >= length list = []
  | otherwise =   take index list ++ [newWord] ++ drop (index + 1) list

replaceDistance :: Int -> [Int] -> [Int]
replaceDistance index list
  | index < 0 || index >= length list = []
  | otherwise =   take index list ++ [0] ++ drop (index + 1) list



takeDamage :: [Bool] -> EventM n TuiState ()
takeDamage hits = do
  let incorrectWord = if hits == [] then True else False
  currentState <- get
  if incorrectWord -- user typed incorrect word
    then 
      put
         ( TuiState
          { tuiStateTarget =  tuiStateTarget currentState ,
            tuiStateInput = tuiStateInput currentState,
            currentScore = currentScore currentState,
            remainingWords = remainingWords currentState ,
            timer = timer currentState,
            distance =  distance currentState ,
            health =  health currentState - 0.1 ,
            difficultyLevel = difficultyLevel currentState,
            announcement = announcement currentState
          }
    )
  else -- ship got hit
    put
      ( TuiState
          { tuiStateTarget =  boolToWord hits (tuiStateTarget currentState) (remainingWords currentState),
            tuiStateInput = tuiStateInput currentState,
            currentScore = currentScore currentState,
            remainingWords =  drop (countBools hits) (remainingWords currentState),
            timer = timer currentState,
            distance =  boolToDistance hits (distance currentState),
            health =  health currentState - 0.1 * fromIntegral (countBools hits),
            difficultyLevel = difficultyLevel currentState,
            announcement = announcement currentState
          }
      )
  checkDead

boolToDistance :: [Bool] -> [Int] -> [Int]
boolToDistance (b:bs) (d:ds) = if b == True then 0 : boolToDistance bs ds else d: boolToDistance bs ds
boolToDistance _ _ = []

countBools :: [Bool] -> Int
countBools boolList = length (filter id boolList)

boolToWord :: [Bool] -> [String] -> [String] -> [String]
boolToWord [] _ _ = []
boolToWord _ [] _ = []
boolToWord _ _ [] = []
boolToWord (b:bs) (w:ws) rems@(r:rs) = if b == True 
  then r : boolToWord bs ws rs
  else w :  boolToWord bs ws rems 

checkDead :: EventM n TuiState ()
checkDead = do
  newState <- get
  if health newState <=0 then
    halt
  else
    put newState

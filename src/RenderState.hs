module RenderState (drawTui) where

import Attributes
import Brick
import Brick.Widgets.Center
import qualified Brick.Widgets.ProgressBar as P
import GameState
import Ships

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts
  | abs (health ts) <= 0.1 = renderGameOverState ts
  | otherwise = case remainingWords ts of
      _ : _ -> renderOngoingGameState ts
      [] -> renderGameEndState ts

renderOngoingGameState :: TuiState -> [Widget ResourceName]
renderOngoingGameState ts = [a]
  where
    inputWord = if tuiStateInput ts == "" then " " else tuiStateInput ts
    targetWord = tuiStateTarget ts
    wordDistances = distance ts
    ann = if fst (announcement ts) /= 0 then snd (announcement ts) else " "
    a =
      str ("Time Left: " <> show (timer ts))
        <=> enemyShip ts targetWord wordDistances
        <=> hCenter (withAttr inputAttr (str inputWord))
        <=> hCenter (withAttr announcementAttr (str ann))
        <=> padTop (Pad 5) (getHealth ts)

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
      str "Health: " <+> healthBar

enemyShip :: TuiState -> [String] -> [Int] -> Widget ResourceName
enemyShip ts wordsToShow wordDistances =
  padTop
    (Pad 2)
    ( evilShip
        <+> enemyShipHelper ts wordsToShow wordDistances
        <+> ship
    )

enemyShipHelper :: TuiState -> [String] -> [Int] -> Widget ResourceName
-- enemyShipHelper _ [] _  = str ""
enemyShipHelper ts (w : ws) (d : ds) = highlightMatchingPart (d) (w) (tuiStateInput ts) <=> str "\n" <=> enemyShipHelper ts ws ds
enemyShipHelper _ _ _ = str ""

highlightMatchingPart :: Int -> String -> String -> Widget ResourceName
highlightMatchingPart dist target input =
  str (replicate (dist - 1) ' ') <+> go target input emptyWidget
  where
    go [] _ acc = acc
    go remaining [] acc = acc <+> str remaining
    go (t : ts) (i : is) acc
      | t == i = go ts is (acc <+> withAttr matchingAttr (str [t]))
      | otherwise = go ts (replicate (length is) ' ') (acc <+> str [t]) -- Space or another character to indicate non-matching part

renderGameEndState :: TuiState -> [Widget ResourceName]
renderGameEndState ts = [str "You have beaten the game! Your final score is: " <+> str (show $ currentScore ts)]

renderGameOverState :: TuiState -> [Widget ResourceName]
renderGameOverState ts = [gameOver <=> (str "You have lost the game! Your final score is: " <+> str (show $ currentScore ts))]

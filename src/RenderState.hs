module RenderState (drawTui) where

import Attributes
import Brick
import qualified Brick.Widgets.ProgressBar as P
import GameState
import Ships

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts =
  if fst (announcement ts) /= 0
    then renderAnnouncement ts
    else case remainingWords ts of
      _ : _ -> renderOngoingGameState ts
      [] -> renderGameEndState ts

renderAnnouncement :: TuiState -> [Widget ResourceName]
renderAnnouncement ts = [a]
  where
    a =
      str (snd (announcement ts))
        <=> str ("Your final score is: " <> show (currentScore ts))

renderOngoingGameState :: TuiState -> [Widget ResourceName]
renderOngoingGameState ts = [a]
  where
    a =
      str ("Level: " <> show (level ts))
        <=> str ("Time Left: " <> show (timer ts))
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
            [ (healthDoneAttr, progressCompleteAttr),
              (healthToDoAttr, progressIncompleteAttr)
            ]
        )
        $ bar
        $ health st
    lbl c = Just $ show $ fromEnum $ c * 100
    bar v = P.progressBar (lbl v) v
    ui =
      str "Health: " <+> healthBar

enemyShip :: TuiState -> String -> Widget ResourceName
enemyShip ts word =
  padTop
    (Pad 2)
    ( evilShip
        <+> str (replicate (distance ts - 1) ' ' <> word)
        <+> ship
    )

renderGameEndState :: TuiState -> [Widget ResourceName]
renderGameEndState ts = [str "You have beaten the game! Your final score is:" <+> str (show $ currentScore ts)]
module RenderState (drawTui) where

import Attributes
import Brick
import qualified Brick.Widgets.ProgressBar as P
import GameState
import Ships

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = case remainingWords ts of
  _ : _ -> renderOngoingGameState ts
  [] -> renderGameEndState ts

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

enemyShip :: TuiState -> [String] -> Widget ResourceName
enemyShip ts wordsToShow =
  padTop
    (Pad 2)
    ( evilShip
        <+> 
        enemyShipHelper ts wordsToShow
        <+> ship
    )

enemyShipHelper :: TuiState -> [String] -> Widget ResourceName
enemyShipHelper _ [] = str ""
enemyShipHelper ts wordsToShow = str (replicate (distance ts - 1) ' ' <> head wordsToShow) <=> str "\n" <=> enemyShipHelper ts (tail wordsToShow)


renderGameEndState :: TuiState -> [Widget ResourceName]
renderGameEndState ts = [str "You have beaten the game! Your final score is:" <+> str (show $ currentScore ts)]
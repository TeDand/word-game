module RenderState (drawTui) where

import Attributes
import Brick
import qualified Brick.Widgets.ProgressBar as P
import GameState
import Ships

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts 
  | health ts <= 0 = renderLossState ts

  | remainingWords ts == [] = renderGameEndState ts
  | otherwise = renderOngoingGameState ts

renderOngoingGameState :: TuiState  -> [Widget ResourceName]
renderOngoingGameState ts = [a]
  where
    a =
      (str $ "Level: " <> (show $ (level ts)))
        <=> (str $ "Time Left: " <> (show $ (timer ts)))
        <=> enemyShip ts targetWord
        <=> str inputWord
        <=> padTop (Pad 3) (getHealth ts)
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
    (   evilShip
        <+>
        streamWords ts word
        <+> ship
    )
streamWords :: TuiState -> String -> Widget ResourceName -- somehow have to get random generation and different words created
streamWords ts word = (str $ replicate ((distance ts)) ' ' <> word) 
                  <=> (str $ replicate ((distance ts)) ' ' <> word)
                   <=> (str $ replicate ((distance ts)) ' ' <> word)
                   <=> (str $ replicate ((distance ts)) ' ' <> word)
                   <=> (str $ replicate ((distance ts)) ' ' <> word)
                   <=> (str $ replicate ((distance ts)) ' ' <> word)
                   <=> (str $ replicate ((distance ts)) ' ' <> word)

renderGameEndState :: TuiState -> [Widget ResourceName]
renderGameEndState ts = [str "You have beaten the game! Your final score is: " <+> str (show $ currentScore ts)]

renderLossState :: TuiState -> [Widget ResourceName]
renderLossState ts = [gameOver <=> str ("Your final score is: " <> (show $ currentScore ts))]

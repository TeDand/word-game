module Attributes (module Attributes) where

import Brick
import qualified Brick.Widgets.ProgressBar as P
import GameState
import Graphics.Vty.Attributes

gameMap :: TuiState -> AttrMap
gameMap _ =
  attrMap
    defAttr
    [ (inputAttr, fg yellow),
      (theBaseAttr, bg brightBlack),
      (healthDoneAttr, blue `on` green),
      (healthToDoAttr, blue `on` red),
      (progressIncompleteAttr, fg yellow),
      (announcementAttr, fg red),
      (matchingAttr, fg green)
    ]
  

inputAttr :: AttrName
inputAttr = attrName "input"

theBaseAttr :: AttrName
theBaseAttr = attrName "theBase"

healthDoneAttr, healthToDoAttr :: AttrName
healthDoneAttr = theBaseAttr <> attrName "health:done"
healthToDoAttr = theBaseAttr <> attrName "health:remaining"

progressIncompleteAttr :: AttrName
progressIncompleteAttr = P.progressCompleteAttr

progressCompleteAttr :: AttrName
progressCompleteAttr = P.progressCompleteAttr

announcementAttr :: AttrName
announcementAttr = attrName "announcement"

matchingAttr :: AttrName
matchingAttr = attrName "matching"
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Game
    (
        game
    )
where  
import Ships

import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, forever)
import Control.Concurrent (threadDelay, forkIO)
import Data.Monoid
import qualified Graphics.Vty as V
import Brick.BChan
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.ProgressBar as P

import Brick

data CustomEvent = MoveDown | Tick deriving Show

data St =
    St { 
        _timer :: Int
       , _distance :: Int
       , _level :: Int
       , _health :: Float
       }
makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = [a]
    where
        a = (str $ "Level: " <> (show $ st^.level))
            <=>
            (str $ "Time Left: " <> (show $ st^.timer))
            <=>
            enemyShip st
            <=>
            padTop (Pad 5) (getHealth st)

getHealth :: St -> Widget ()
getHealth st = ui
    where
        healthBar = updateAttrMap
            (A.mapAttrNames [(healthDoneAttr, P.progressCompleteAttr)
                            , (healthToDoAttr, P.progressIncompleteAttr)])
             $ bar $ _health st
        lbl c = Just $ show $ fromEnum $ c * 100
        bar v = P.progressBar (lbl v) v
        ui = 
            (str "Health: " <+> healthBar)           
          
enemyShip :: St -> Widget ()
enemyShip st =  padTop (Pad 2)
            (evilShip
            <+>

            (str $ replicate (st^.distance-1) ' ' <> "word")
            
            <+>
            ship)



appEvent :: BrickEvent () CustomEvent -> EventM () St ()
appEvent e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt
        AppEvent MoveDown -> distance %= (\row -> if row < 80 then row + 1 else 0)
        AppEvent Tick -> do
            timer %= (\c -> if c > 0 then c - 1 else 0)
            
        _ -> return ()

initialState :: St
initialState =
    St {  _timer = 20
       , _distance = 0
       , _level = 1
       , _health = 1.0
       }

theBaseAttr :: A.AttrName
theBaseAttr = A.attrName "theBase"

healthDoneAttr, healthToDoAttr :: A.AttrName
healthDoneAttr = theBaseAttr <> A.attrName "health:done"
healthToDoAttr = theBaseAttr <> A.attrName "health:remaining"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
         [ (theBaseAttr,               bg V.brightBlack)
         , (healthDoneAttr,                 V.blue `on` V.green)
         , (healthToDoAttr,                 V.blue `on` V.red)
         , (P.progressIncompleteAttr,  fg V.yellow)
         ]

theApp :: App St CustomEvent ()
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const theMap
        }

game :: IO ()
game = do
    chan <- newBChan 10

    void $ forkIO $ forever $ do
        writeBChan chan MoveDown
        threadDelay 100000 -- enemy

    void $ forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 1000000 -- game timer 

    void $ customMainWithDefaultVty (Just chan) theApp initialState
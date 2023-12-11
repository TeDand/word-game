module Menu
  ( menu,
  )
where

import Brick
import Scoreboard
import qualified Graphics.Vty as V

dummy :: Int -> IO Int
dummy x = return x

tr :: String -> Widget ()
tr x = str x

menu :: Int -> IO ()
menu i = do
  x <- topMain $ tr top
  sb <- readScoreboard
  case x of
    "b" -> do 
            s <- dummy i
            n <- nameMain $ tr name
            if n/="" then writeScoreboard (addNewScore sb n s) else return ()
            menu i
    "s" -> do
            simpleMain $ tr $ "press any key to quit\n" ++ (show sb)
            menu i
    "o" -> do
            d <- diffMain i $ tr diff
            menu d
    "q" -> return ()
    _ -> error "?"

top :: String
top = "Hello, welcome to play Wordgame!\n\
      \press b to begin play\n\
      \press s to see the scoreboard\n\
      \press o to change option\n\
      \press q to quit."

topMain :: Widget () -> IO String
topMain w = defaultMain (topApp w) ""

topApp :: Widget () -> App String e ()
topApp w =
    App { appDraw = const [w]
        , appHandleEvent = topHandle
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap V.defAttr []
        , appChooseCursor = neverShowCursor
        }

topHandle :: BrickEvent () e -> EventM () String ()
topHandle (VtyEvent (V.EvKey (V.KChar 'b') [])) = do put "b"; halt
topHandle (VtyEvent (V.EvKey (V.KChar 's') [])) = do put "s"; halt
topHandle (VtyEvent (V.EvKey (V.KChar 'o') [])) = do put "o"; halt
topHandle (VtyEvent (V.EvKey (V.KChar 'q') [])) = do put "q"; halt
topHandle _ = continueWithoutRedraw

name :: String
name = "Thank you for playing.\n\
        \Type your name to save your score in the scoreborad.\n\
        \Click esc when you finish.\n\
        \If you don't want to save your score, click esc without type your name."

nameMain :: Widget () -> IO String
nameMain w = defaultMain (nameApp w) ""

nameApp :: Widget () -> App String e ()
nameApp w =
    App { appDraw = const [w]
        , appHandleEvent = nameHandle
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap V.defAttr []
        , appChooseCursor = neverShowCursor
        }

nameHandle :: BrickEvent () e -> EventM () String ()
nameHandle (VtyEvent (V.EvKey V.KEsc [])) = halt
nameHandle (VtyEvent (V.EvKey (V.KChar c) [])) = do s <- get; put $ s++[c]
nameHandle _ = continueWithoutRedraw

diff :: String
diff= "Input a number to select the difficulty level\n\
      \Press esc to cancel."

diffMain :: Int -> Widget () -> IO Int
diffMain i w = defaultMain (diffApp w) i

diffApp :: Widget () -> App Int e ()
diffApp w =
    App { appDraw = const [w]
        , appHandleEvent = diffHandle
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap V.defAttr []
        , appChooseCursor = neverShowCursor
        }

diffHandle :: BrickEvent () e -> EventM () Int ()
diffHandle (VtyEvent (V.EvKey V.KEsc [])) = halt
diffHandle (VtyEvent (V.EvKey (V.KChar '0') [])) = do put 0; halt
diffHandle (VtyEvent (V.EvKey (V.KChar '1') [])) = do put 1; halt
diffHandle (VtyEvent (V.EvKey (V.KChar '2') [])) = do put 2; halt
diffHandle _ = continueWithoutRedraw
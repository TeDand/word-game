module Menu
  ( menu,
  )
where

import Brick
import Brick.Widgets.Center
import Dataloader
import qualified Graphics.Vty as V
import Scoreboard
import Tui

-- dummy :: Difficulty -> IO Int
-- dummy _ = return 0

tr :: String -> Widget ()
tr x = str x

fm :: Int -> Difficulty
fm i = case i of
  1 -> Easy
  2 -> Hard
  3 -> Nightmare
  _ -> error "Please choose a valid difficulty level"

menu :: Int -> IO ()
menu i = do
  x <- defaultMain topApp ""
  sb <- readScoreboard
  case x of
    "b" -> do
      s <- tui $ fm i
      n <- defaultMain nameApp ""
      if n /= "" then writeScoreboard (addNewScore sb n s) else return ()
      menu i
    "s" -> do
      simpleMain $ hCenter (tr $ sbMenu (show sb))
      menu i
    "o" -> do
      d <- defaultMain diffApp i
      menu d
    "q" -> return ()
    _ -> error "?"

sbMenu :: String -> String
sbMenu scores =
  "╔═══════════════════════════════════╗\n\
  \║           Scoreboard              ║\n\
  \╚═══════════════════════════════════╝\n\
  \ \n"
    ++ scores
    ++ "\
       \• Press 'esc' to cancel.\n\
       \ \n\
       \═════════════════════════════════════"

top :: String
top =
  "╔═══════════════════════════════════╗\n\
  \║         Wordgame Main Menu        ║\n\
  \╚═══════════════════════════════════╝\n\
  \ \n\
  \    • Press 'b' to begin the game\n\
  \    • Press 's' to see the scoreboard\n\
  \    • Press 'o' to change options\n\
  \    • Press 'q' to quit\n\
  \ \n\
  \═════════════════════════════════════"

topApp :: App String e ()
topApp =
  App
    { appDraw = const [hCenter (str top)],
      appHandleEvent = topHandle,
      appStartEvent = return (),
      appAttrMap = const $ attrMap V.defAttr [],
      appChooseCursor = neverShowCursor
    }

topHandle :: BrickEvent () e -> EventM () String ()
topHandle (VtyEvent (V.EvKey (V.KChar 'b') [])) = do put "b"; halt
topHandle (VtyEvent (V.EvKey (V.KChar 'B') [])) = do put "b"; halt
topHandle (VtyEvent (V.EvKey (V.KChar 's') [])) = do put "s"; halt
topHandle (VtyEvent (V.EvKey (V.KChar 'S') [])) = do put "s"; halt
topHandle (VtyEvent (V.EvKey (V.KChar 'o') [])) = do put "o"; halt
topHandle (VtyEvent (V.EvKey (V.KChar 'O') [])) = do put "o"; halt
topHandle (VtyEvent (V.EvKey (V.KChar 'q') [])) = do put "q"; halt
topHandle (VtyEvent (V.EvKey (V.KChar 'Q') [])) = do put "q"; halt
topHandle _ = continueWithoutRedraw

name :: String
name =
  "╔══════════════════════════════════════════════════╗\n\
  \║               Thank you for playing!             ║\n\
  \╚══════════════════════════════════════════════════╝\n\
  \ \n\
  \• Type your name to save your score.\n\
  \• Press 'esc' when you finish.\n\
  \• If you don't want to save your score, press 'esc'.\n\
  \ \n\
  \═══════════════════════════════════════════════════"

nameApp :: App String e String
nameApp =
  App
    { appDraw = \s -> [hCenter (str name <=> str s)],
      appHandleEvent = nameHandle,
      appStartEvent = return (),
      appAttrMap = const $ attrMap V.defAttr [],
      appChooseCursor = neverShowCursor
    }

nameHandle :: BrickEvent String e -> EventM String String ()
nameHandle (VtyEvent (V.EvKey V.KEsc [])) = halt
nameHandle (VtyEvent (V.EvKey (V.KChar c) [])) = do s <- get; put $ s ++ [c]
nameHandle _ = continueWithoutRedraw

diff :: String
diff =
  "╔══════════════════════════════════════════════╗\n\
  \║             Select Difficulty Level          ║\n\
  \╚══════════════════════════════════════════════╝\n\
  \ \n\
  \• Input a number to select the difficulty level:\n\
  \  1: Easy\n\
  \  2: Hard\n\
  \  3: Nightmare\n\
  \• Press 'esc' to cancel.\n\
  \ \n\
  \════════════════════════════════════════════════"

diffApp :: App Int e ()
diffApp =
  App
    { appDraw = const [hCenter (str diff)],
      appHandleEvent = diffHandle,
      appStartEvent = return (),
      appAttrMap = const $ attrMap V.defAttr [],
      appChooseCursor = neverShowCursor
    }

diffHandle :: BrickEvent () e -> EventM () Int ()
diffHandle (VtyEvent (V.EvKey V.KEsc [])) = halt
diffHandle (VtyEvent (V.EvKey (V.KChar '1') [])) = do put 1; halt
diffHandle (VtyEvent (V.EvKey (V.KChar '2') [])) = do put 2; halt
diffHandle (VtyEvent (V.EvKey (V.KChar '3') [])) = do put 3; halt
diffHandle _ = continueWithoutRedraw
module Main (main) where

import Tui


main :: IO ()
main = do {
    putStrLn "Press Enter to Start the Game";
    _ <- getLine;
    tui
}
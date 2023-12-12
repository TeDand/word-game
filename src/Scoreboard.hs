module Scoreboard
  ( readScoreboard,
  writeScoreboard,
  addNewScore,
  )
where

import Text.Parsec hiding (Empty)
import Text.Parsec.String

data ScoreBoard
   = Sb String Int ScoreBoard -- name, score, next person
   | Empty -- end of scoreboard

instance Show ScoreBoard where
  show (Sb a b c)=show a ++ " " ++ show b ++"\n" ++ show c
  show (Empty)=""

sbp :: Parser ScoreBoard
sbp = try (do
  _ <- many (oneOf "\"")
  n <- many (noneOf "\" ")
  _ <- many (oneOf "\"")
  _ <- space
  s <- many (digit)
  _ <- oneOf "\n"
  u <- sbp
  return (Sb n (read s::Int) u))
  <|>
  return Empty

readScoreboard :: IO ScoreBoard
readScoreboard = do
  input <- readFile "sb.txt"
  case (parse sbp "sb.txt" input) of
   (Right x) -> return x
   _ -> error "no scoreboard"

writeScoreboard :: ScoreBoard -> IO ()
writeScoreboard sb = writeFile "sb.txt" (show sb)

addNewScore :: ScoreBoard -> String -> Int -> ScoreBoard
addNewScore (Sb a b c) n i = do
  (if a==n then
    (if i>=b then
      Sb a i c
    else
      Sb a b c)
  else
    (if i>=b then
      Sb n i (Sb a b (rmName c n))
    else
      Sb a b (addNewScore c n i)))
addNewScore Empty n i = Sb n i Empty

rmName :: ScoreBoard -> String -> ScoreBoard
rmName (Sb a b c) n = do
  if a==n then
    c
  else
    Sb a b (rmName c n)
rmName Empty _ = Empty
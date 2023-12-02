module Hello
  ( hello,
  )
where

import Brick

ui :: Widget ()
ui = str "Hello, world!"

hello :: IO ()
hello = simpleMain ui
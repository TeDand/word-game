{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Types (Enemy (..)) where

data Enemy = Enemy
  { _enemyWord :: String,
    _distance :: Int,
    _row :: Int
  }

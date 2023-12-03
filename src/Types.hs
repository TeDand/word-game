{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Types where
import Control.Lens
import qualified Data.Text                     as T
data EnemyWord = EnemyWord {
     _typed :: T.Text
    , _untype :: T.Text
}
makeLenses '' EnemyWord

data Enemy = Enemy {
     _enemyWord :: EnemyWord
    , _health :: Int
    , _damage :: Float
    , _distance :: Int

}
makeLenses '' Enemy

data Health = Health {
     _healthValue :: Float
    , _healthMax :: Float
}
makeLenses '' Health
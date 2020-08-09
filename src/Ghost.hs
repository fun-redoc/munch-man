{-# LANGUAGE TemplateHaskell, DuplicateRecordFields #-}
module Ghost where

import Control.Lens
import System.Random
import Lib

data Ghost = Ghost {_object::RectEntity
                   , _speed::Float
                   , _g::StdGen 
                   } deriving (Show)
instance Eq Ghost where
  g1@Ghost {_object=o1, _speed=s1} == g2@Ghost {_object=o2, _speed=s2} = o1 == o2 && s1 == s2
makeLenses ''Ghost

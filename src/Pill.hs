module Pill where

import Lib
  
data Pill 
  = YellowPill CircleEntity
  | BluePill CircleEntity
  deriving (Show, Eq)  


score::Pill->Int
score (YellowPill _) = 1
score (BluePill   _) = 5

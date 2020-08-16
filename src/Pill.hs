module Pill where

import Lib
  
data Pill 
  = YellowPill CircleEntity
  | BluePill CircleEntity
  deriving (Show, Eq)  


pillScore::Pill->Int
pillScore (YellowPill _) = 1
pillScore (BluePill   _) = 5

pillCircleEntity::Pill->CircleEntity 
pillCircleEntity (YellowPill ce) = ce
pillCircleEntity (BluePill ce) = ce
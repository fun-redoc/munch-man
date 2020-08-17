module Event where

import Lib


data GameEvent
  = GameEventManGo     Dir Time 
  | GameEventManStop   Time
  | GameEventStartGame Time
  | GameEventQuit      Time
  | GameEventNoOp      Time
  deriving (Show, Eq)

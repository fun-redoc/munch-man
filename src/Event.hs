module Event where

import Lib


data GameEvent
  = GameEventManGo Dir
  | GameEventManStop 
  | GameEventStartGame
  | GameEventQuit 
  | GameEventNoOp
  deriving (Show, Eq)

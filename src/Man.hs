{-# LANGUAGE TemplateHaskell #-}
module Man where
import Control.Lens
import Control.Monad.Trans.State.Strict

import Paths_munchman_gloss
import Lib
import Step
import Event
import Pill
import Ghost

type DigestTime = Float
type DyingTime = Float
type ManShape = CircleEntity

data ManAction
  = ManActionStop Dir
  | ManActionGo   Dir 
  | ManActionEat  Pill Dir DigestTime           -- ^ digest time: how long the mouth is open
  | ManActionGhostCollition Ghost Dir DyingTime
  deriving (Show, Eq)

data ManState = ManState
              { _action::ManAction
              , _score::Int
              , _speed::Float
              , _object::ManShape
              , _lastState::Maybe ManState
              } deriving (Show, Eq)
makeLenses ''ManState
mkManState::Maybe CircleEntity -> ManState
mkManState manObject = ManState (ManActionStop DirRight) 0 2.0 (maybe (0.0,0.0,0.4) id manObject) Nothing

updateManAction::ManState->ManAction->ManState
updateManAction manState manAction = manState&action .~ manAction
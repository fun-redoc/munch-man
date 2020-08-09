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
              , _yellow::Int
              , _blue::Int
              , _speed::Float
              , _object::ManShape
              } deriving (Show, Eq)
makeLenses ''ManState
mkManState::ManState
mkManState = ManState (ManActionStop DirRight) 0 0 1.0 (0.0,0.0,0.5)

updateManAction::ManState->ManAction->ManState
updateManAction manState manAction = manState&action .~ manAction
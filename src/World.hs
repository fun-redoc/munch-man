{-# LANGUAGE BlockArguments, TemplateHaskell, BangPatterns, GADTs, ScopedTypeVariables #-}
module World where

import Lib
import Man
import Event
import Step
import Pill
import Game

import Control.Lens

data World  = World { _time::Float
                    , _event::Time->GameEvent
                    , _scene::GameScene
                    }
makeLenses ''World

score world = Game.score (world^.scene)
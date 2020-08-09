{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Lib where

import Data.Maybe (isNothing)
import Data.List
import Control.Lens
import Paths_munchman_gloss
import System.Random
import qualified Data.Vector as V


someFunc::IO ()
someFunc = putStrLn "NOT IMPLEMENTED"


data Dir
  = DirLeft
  | DirRight
  | DirUp
  | DirDown
  deriving (Show, Eq)

type AbsVelocity = Float
type DeltaTime = Float
--type Field2d a = [[a]]                         -- ^ used for reading level file
type Pos = (Int,Int)
type Vec = (Float,Float)                       -- ^ dx , dy
type Position = Vec
type Velocity = Vec

class Entity e where
  move::DeltaTime->Dir->AbsVelocity->e->e

type CircleEntity = (Float, Float, Float)      -- ^ center x, center y, radius
instance Entity CircleEntity where
  move dt DirLeft  vel (x,y,r) = (x-vel*dt,y,r)
  move dt DirRight vel (x,y,r) = (x+vel*dt,y,r)
  move dt DirUp    vel (x,y,r) = (x,y-vel*dt,r)
  move dt DirDown  vel (x,y,r) = (x,y+vel*dt,r)

type RectEntity = (Float, Float, Float, Float) -- ^ topleft x, topleft y, width, height
instance Entity RectEntity where
  move dt DirLeft  vel (x,y,w,h) = (x-vel*dt,y,w,h)
  move dt DirRight vel (x,y,w,h) = (x+vel*dt,y,w,h)
  move dt DirUp    vel (x,y,w,h) = (x,y-vel*dt,w,h)
  move dt DirDown  vel (x,y,w,h) = (x,y+vel*dt,w,h)



--data Direction = GoLeft | GoRight  | GoUp | GoDown | Stop Direction
----data Man = Man {_manEntity::CircleEntity, _manSpeed::AbsVelocity, _dir::Direction}
----makeLenses ''Man
----data Ghost = Ghost {_ghostPos::Position, _vel::Velocity, _g::StdGen}
----makeLenses ''Ghost
--
--
--toDirVec::Direction->AbsVelocity->Velocity
--toDirVec (Stop _) _    = (0,0)
--toDirVec GoLeft speed  = (-speed, 0)
--toDirVec GoRight speed = (speed, 0)
--toDirVec GoUp speed    = (0, speed)
--toDirVec GoDown speed  = (0, -speed)

add::(Num a)=>(a,a)->(a,a)->(a,a)
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)


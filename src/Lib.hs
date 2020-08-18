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

type AbsVelocity = Float
type DeltaTime = Float
type Time = Float
--type Field2d a = [[a]]                         -- ^ used for reading level file
type Pos = (Int,Int)
type Vec = (Float,Float)                       -- ^ dx , dy
type Position = Vec
type Velocity = Vec

data Dir
  = DirLeft
  | DirRight
  | DirUp
  | DirDown
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Random Dir where
  random g = randomR (minBound::Dir, maxBound::Dir) g
  randomR (l,h) g = (\(i,g')->(toEnum i, g')) (System.Random.randomR (fromEnum l, fromEnum h) g)

class Entity e where
  position::e->Vec
  move::DeltaTime->Dir->AbsVelocity->e->e
  moveToDest::DeltaTime->Vec->AbsVelocity->e->e

type CircleEntity = (Float, Float, Float)      -- ^ center x, center y, radius
instance Entity CircleEntity where
  position (x,y,_) = (x,y)
  move dt DirLeft  vel (x,y,r) = (x-vel*dt,y,r)
  move dt DirRight vel (x,y,r) = (x+vel*dt,y,r)
  move dt DirUp    vel (x,y,r) = (x,y-vel*dt,r)
  move dt DirDown  vel (x,y,r) = (x,y+vel*dt,r)
  moveToDest dt (dx,dy) vel (x,y,r) = (x+dx*dt*vel, y+dy*dt*vel, r)

type RectEntity = (Float, Float, Float, Float) -- ^ topleft x, topleft y, width, height
instance Entity RectEntity where
  position (x,y,_,_) = (x,y)
  move dt DirLeft  vel (x,y,w,h) = (x-vel*dt,y,w,h)
  move dt DirRight vel (x,y,w,h) = (x+vel*dt,y,w,h)
  move dt DirUp    vel (x,y,w,h) = (x,y-vel*dt,w,h)
  move dt DirDown  vel (x,y,w,h) = (x,y+vel*dt,w,h)
  moveToDest dt (dx,dy) vel (x,y,w,h) = (x+dx*dt*vel, y+dy*dt*vel, w,h)

class RoughlyEq a where
  (~=)::Float->a->a->Bool
  roughlyEq::Float->a->a->Bool
  roughlyEq = (~=)
  {-# MINIMAL (~=) #-}

instance RoughlyEq Vec where
  (~=) err (x1,y1) (x2,y2) = (err*err) > ((x1-x2)**2 + (y1-y2)**2)

add::(Num a)=>(a,a)->(a,a)->(a,a)
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

distSqrd::Vec->Vec->Float
distSqrd (x1,y1) (x2,y2) = ((x1-x2)**2 + (y1-y2)**2)


randomPick::(RandomGen g)=>g->[a]->(a,g)
randomPick g xs = (xs !! i, g')
  where (i, g') = randomR (0, (length xs)-1) g


findNeighbours::Float->[Vec]->Vec->[Vec]
findNeighbours dist xs x = filter ((~=) dist x) xs
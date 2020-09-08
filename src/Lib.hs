{-# LANGUAGE TemplateHaskell
           , FlexibleInstances
           , MultiParamTypeClasses 
           , FlexibleContexts
           , ScopedTypeVariables
#-}


module Lib where

import Debug.Trace (trace)

import Data.Maybe (isNothing, isJust, fromJust, isNothing)
import Data.List
import Control.Lens
import Paths_munchman_gloss
import System.Random
import qualified Data.Vector as V

import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R
import qualified Data.Array.Repa.Shape as R
import qualified Data.Array.Repa.Repr.Vector as R

import GHC.Base (join)
import qualified Data.HashMap.Lazy as M
import qualified Data.HashSet as S
import Data.Hashable (Hashable)
import Data.Semigroup


import Data.Graph.Inductive.Graph (Node, mkGraph, lab, labNodes)
import Data.Graph.Inductive.PatriciaTree (Gr, UGr)
import Data.Graph.Inductive.Query.SP (sp, spLength)
import Data.List (elemIndex, nub, sort, find)
import Data.Maybe (fromJust)
import PriorityQueue (Infinite)
import Graph 
import WGraph

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

data GG p a = GG {unGG::(R.Array R.V R.DIM2) (Maybe (a, p))}

toRepaDIM2::[String] -> GG (Infinite Float) Pos 
toRepaDIM2 xs = GG $ R.computeS $ R.traverse
                  (R.fromListUnboxed (R.Z R.:. (rows::Int) R.:. (cols::Int)) (join $ reverse xs))
                  id
                  (\f (R.Z R.:. y R.:. x)->extract ((x,y),f (R.ix2 y x)))
    where extract ((x,y),c) = 
            if (c == 'O' || c == '.' || c == '_' || c == 'G' || c == '@')
                then Just ((x,y),1)
                else Nothing
          rows = length xs
          cols = if rows > 0 then (length $ xs !! 0) else 0

adjacent_vertices_GG (GG g) (x,y) = let [nCols, nRows] = R.listOfShape $ R.extent g
                                        xMinus1 = x - 1
                                        xPlus1  = x + 1
                                        yMinus1  = y - 1
                                        yPlus1  = y + 1
                                        xLeft   = if xMinus1 == -1 then nCols-1 -- tunnel
                                                                  else xMinus1
                                        xRight  = if xPlus1 == nCols then 0 -- tunnel
                                                                    else xPlus1
                                        yUp     = if yPlus1 == nRows then 0 -- tunnel
                                                                    else yPlus1
                                        yDown   = if yMinus1 == -1 then nRows-1 -- tunnel
                                                                  else yMinus1
                                        leftNeighbour  = g R.! (R.ix2 y     xLeft )
                                        rightNeighbour = g R.! (R.ix2 y     xRight)
                                        upperNeighbour = g R.! (R.ix2 yUp   x)
                                        lowerNeighbour = g R.! (R.ix2 yDown x)
                                    in (map fromJust . filter isJust) $ [leftNeighbour, rightNeighbour, upperNeighbour, lowerNeighbour]


instance (Num n)=>Graph (GG  n) Pos where
  emptyGraph       = undefined
  num_vertices     = undefined
  adjacent_vertices= undefined
  all_nodes        = undefined
  add_vertex       = undefined

instance Num n=>WGraph GG n Pos where
  get_weight (GG g) v1 v2    = 1 
  adjacent_vertices = adjacent_vertices_GG


type FieldGraph = Gr Pos Int

shortest_path::FieldGraph->Pos->Pos->Maybe [Pos]
shortest_path g start dest = 
    let ln = labNodes g
        node lab = fst <$> find (\(_,l)->l==lab) ln
        start'::Maybe Node = node start
        dest' ::Maybe Node = node dest
        sp'  = start' >>= (\ start''-> dest' >>= (\dest'' -> (sp start'' dest'' g)))
    in (map (fromJust . lab g)) <$> sp' 


add::(Num a)=>(a,a)->(a,a)->(a,a)
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

distSqrd::Vec->Vec->Float
distSqrd (x1,y1) (x2,y2) = ((x1-x2)**2 + (y1-y2)**2)


randomPick::(RandomGen g)=>g->[a]->(a,g)
randomPick g xs = (xs !! i, g')
  where (i, g') = randomR (0, (length xs)-1) g


findNeighbours::Float->[Vec]->Vec->[Vec]
findNeighbours dist xs x = filter ((~=) dist x) xs
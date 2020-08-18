{-# LANGUAGE TemplateHaskell, DuplicateRecordFields, ScopedTypeVariables #-}
module Ghost where

import Debug.Trace (trace)

import Data.List
import Data.Maybe
import Control.Lens
import System.Random
import Lib

data Ghost = Ghost {_object::RectEntity
                   , _dest::Vec
                   , _dir::Vec
                   , _speed::Float
                   , _randomGen::StdGen 
                   } deriving (Show)
instance Eq Ghost where
  g1@Ghost {_object=o1, _speed=s1} == g2@Ghost {_object=o2, _speed=s2} = o1 == o2 && s1 == s2
makeLenses ''Ghost

mkGhost :: RealFrac a => a -> [Vec] -> RectEntity  -> Ghost
mkGhost f path ghostObject@(x,y,w,h) = Ghost ghostObject dest' dir' 1.0 rg'
  where
    rg = (mkStdGen (round f))
    neighbours = findNeighbours 1.01 path (x,y)
    ((x',y'), rg') = randomPick rg neighbours
    dest' = (x',y')
    dir' = (x'-x,y'-y)

moveGhostOnRails::Entity e=>e->[Ghost]->[Vec]->DeltaTime->Ghost->Ghost
moveGhostOnRails man allGhosts path dt ghost =
  let rg = ghost^.randomGen
      (x,y,w,h) = ghost^.object
      allGhostsPositions = map (\Ghost{_object=(x,y,_,_)}->(x,y)) allGhosts
      destReached = (~=) 0.01 (ghost^.dest) (x,y)
      manPos = position man
      neighbours' = findNeighbours 1.1 (path \\ allGhostsPositions) (x,y)
      neighbours'' = findNeighbours 1.1 (delete (x,y) path) (x,y)
      ((x'',y''), rg'') = randomPick rg neighbours''
      ((x''', y'''), rg''') = if null neighbours'
                        --then ((x'', y''), rg'')
                        then (minimumBy (\n1 n2-> compare (distSqrd n1 manPos) (distSqrd n2 manPos) ) neighbours''
                             ,rg
                             )
                        else (minimumBy (\n1 n2-> compare (distSqrd n1 manPos) (distSqrd n2 manPos) ) neighbours'
                             ,rg
                             )
      ((x',y'), rg') = randomPick rg''' ((x''',y'''):neighbours') -- [(x'',y''), (x''',y''')]
      dest' = (x',y')
      dir' = (x'-x, y'-y)
      ghost1 = ghost&randomGen .~ rg'
  in if destReached
       then ghost1&dest .~ dest'
                  &dir  .~ dir'
                  &object %~ moveToDest dt dir' (ghost^.speed)
       else ghost1&object %~ moveToDest dt (ghost^.dir) (ghost^.speed)
      
      

moveGhostRandomly::DeltaTime->Ghost->Ghost
moveGhostRandomly dt ghost = let rg = ghost^.randomGen 
                                 (dir, rg') = random rg
                              in ghost&randomGen .~ rg'
                                      &object    %~ (move dt dir (ghost^.speed))
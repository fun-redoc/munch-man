{-# LANGUAGE TemplateHaskell, DuplicateRecordFields, ScopedTypeVariables #-}
module Ghost where

import Debug.Trace (trace)

import qualified Data.Array.Repa as R
import Data.List
import Data.Maybe
import Control.Lens
import System.Random
import Lib
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.Vector as R
import Data.Graph.Inductive.Graph (delNode, nodes, Node, mkGraph, lab)
import Data.Graph.Inductive.PatriciaTree (Gr, UGr)
import Data.Graph.Inductive.Query.SP (sp, spLength)
import Data.List (elemIndex, nub, sort)
import Data.Maybe (fromJust)

data Ghost = Ghost {_object::RectEntity
                   , _dest::Vec
                   , _dir::Vec
                   , _speed::Float
                   , _fieldGraph::FieldGraph
                   , _randomGen::StdGen 
                   } -- deriving (Show)
instance Eq Ghost where
  g1@Ghost {_object=o1, _speed=s1} == g2@Ghost {_object=o2, _speed=s2} = o1 == o2 && s1 == s2
makeLenses ''Ghost
instance Show Ghost where
  show g1@Ghost {_object=o1, _speed=s1, _dest=de, _dir=di} = 
        "Ghost {object=" ++ (show o1) ++ "}" ++
              "{speed"   ++ (show s1) ++ "}" ++
              "{dest="   ++ (show de) ++ "}" ++
              "{dir="    ++ (show di) ++ "}" 

-- mkGhost :: RealFrac a => a -> [Vec] -> R.Array R.V R.DIM2 (Maybe Pos) -> RectEntity  -> Ghost
mkGhost :: RealFrac a => a -> [Vec] -> FieldGraph -> RectEntity  -> Ghost
mkGhost f path fieldGraph ghostObject@(x,y,w,h) = Ghost ghostObject dest' dir' 1.0 fieldGraph rg'
  where
    rg = (mkStdGen (round f))
    neighbours = findNeighbours 1.01 path (x,y)
    ((x',y'), rg') = randomPick rg neighbours
    dest' = (x,y)
    dir' = (0,0)

score::Ghost->Int
score = const 3

moveGhostOnRepaPath::Entity e=>e->[Vec]->DeltaTime->Ghost->Ghost
moveGhostOnRepaPath man path dt ghost = -- (trace "Hallo") $
  if destReached
    then ghost&dir    .~ newDir
              &dest   .~ newDest
              &randomGen .~ g
              &object %~ moveToDest dt newDir (ghost^.speed)
    else ghost&object %~ moveToDest dt (ghost^.dir) (ghost^.speed)
    where destReached = -- (\dr-> (trace $ "Dest Reached"++(show dr)) $ dr) $ 
                        (~=) 0.01 (ghost^.dest) (gx,gy)
          (gx,gy,_,_) = ghost^.object
          (mx,my) = over both truncate (position man)
          (destx, desty) = over both truncate (ghost^.dest)
          newDest::Vec
          newDest@(x',y') = over both fromIntegral newDestPos
          newDir::Vec
          newDir = (x'-gx, y'-gy)
          newDestPos::Pos
          newDestPos = if isJust sp' --length sp > 1
                        then if length sp > 1
                                then sp !! 1
                                else (destx, desty)
                        else (destx,desty)
          (fieldGraphPruned,g) = foldr (\n (rg, g) -> 
                                          let (p::Float,g') = randomR (0,1) g
                                           in if p < 0.95 then (rg,g')
                                                          else (delNode n rg, g') -- randomly remove 10 nodes
                                       ) 
                                       (ghost^.fieldGraph, ghost^.randomGen) 
                                       (nodes (ghost^.fieldGraph))
          sp'::Maybe [Pos] = -- (\s-> (trace $ show s) $ s) $ 
                      -- shortest_path (ghost^.fieldGraph) (destx,desty) (mx,my)
                      shortest_path fieldGraphPruned (destx,desty) (mx,my)
          sp = fromJust sp'
           

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

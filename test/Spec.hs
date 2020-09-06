{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
import Graph
import Lib
import Board
--import Data.Graph.Inductive.Graph (Node, mkGraph, lab, labNodes)
--import Data.Graph.Inductive.PatriciaTree (Gr, UGr)
--import Data.Graph.Inductive.Query.SP (sp, spLength, dijkstra)
import Data.List (elemIndex, nub, sort,find)
import Data.Maybe (isJust, fromJust)
import qualified Data.Array.Repa as R
import Data.Graph.Inductive (undir)
import Graph as G 
import WGraph as WG
import PriorityQueue as PQ
import Control.Monad.Trans.State.Lazy -- (put, get, StateT)
import Data.Map as M
import Data.HashMap.Lazy as HM
import Control.Monad.Loops
import Data.Maybe (isNothing, fromJust)
import Data.Traversable (for)
import Control.Monad (forM_, forM)
import qualified Data.Vector as V
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R
import qualified Data.Array.Repa.Shape as R
import qualified Data.Array.Repa.Repr.Vector as R

import AStar
import GHC.Base (join)

data RG p a = RG {unRG::(R.Array R.V R.DIM2) (Maybe (a, p))}

instance (Num n)=>Graph (RG  n) Pos where
  emptyGraph       = undefined
  num_vertices     = undefined
  adjacent_vertices= undefined
  all_nodes        = undefined
  add_vertex       = undefined

instance Num n=>WGraph RG n Pos where
  get_weight (RG g) v1 v2    = 1 
  adjacent_vertices (RG g) p = adjacent_vertices_RG g p

adjacent_vertices_RG g (x,y) = let [nRows, nCols] = R.listOfShape $ R.extent g
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
                                in -- (\ns -> (trace (show ns)) ns) $ 
                                  (Prelude.map fromJust . Prelude.filter isJust) $ [leftNeighbour, rightNeighbour, upperNeighbour, lowerNeighbour]

toRepaDIM2_RG::[String] -> RG (Infinite Float) Pos 
toRepaDIM2_RG xs = RG $ R.computeS $ R.traverse
                  (R.fromListUnboxed (R.Z R.:. (rows::Int) R.:. (cols::Int)) (join $ reverse xs)) -- TODO take sizes from configuration
                  id
                  (\f (R.Z R.:. y R.:. x)->extract ((x,y),f (R.ix2 y x)))
    where extract ((x,y),c) = -- (trace $ show ("Board", x,y,c)) $
            if (c == 'O' || c == '.' || c == '_' || c == 'G' || c == '@')
                then Just ((x,y),1)
                else Nothing
          rows = length xs
          cols = if rows > 0 then (length $ xs !! 0) else 0

-- euclidian1::(Integral n, Num n)=>Heuristics g (n, n) (Infinite Float)
euclidian1 g (x1,y1) (x2,y2) = sqrt' $ fromIntegral $ (dx*dx + dy*dy)
                               where dx = x2-x1
                                     dy = y2-y1
                                     sqrt' (Bound x) = Bound (sqrt x)
                                     sqrt' PositiveInfinity = PositiveInfinity
                                     sqrt' NegativeInfinity = undefined


test::[String]
test = ["XXXXXXXXXXX"--4
       ,"X_________X"--3
       ,"X___XX____X"--2
       ,"X_________X"--1
       ,"XXXXXXXXXXX"--0
       --01234567890
       ]

instance Bounded (Infinite n) where
  maxBound = PositiveInfinity
  minBound = NegativeInfinity

main :: IO ()
main = do
    let !repa = toRepaDIM2_RG test
    let a1::StateT (PQ [] (Infinite Float) Pos, M.Map Pos (Maybe Pos,(Infinite Float)),RG (Infinite Float) Pos) IO [Pos] 
            = astar euclidian1 (1,1) (9,3)
    print "before"
    print $ WG.adjacent_vertices repa (1,1)
    print "before 1"
    !astarPath <- evalStateT a1 
                             ( emptyPriorityQueue::PQ [] (Infinite Float) Pos
                             , M.empty::M.Map Pos (Maybe Pos,(Infinite Float))
                             , repa
                             )
    print "after"
    print astarPath
    return ()

--main = do
--    let !repa = toRepaDIM2 board
--    let ns::[Pos] = map snd $ map fromJust $ filter isJust (R.toList (unGG repa))
--    let vertices = nub 
--                 $ foldr 
--                    (\n vs -> let neighbours = map snd $ adjacent_vertices_GG repa n
--                                  vs' = sort $ map (\p -> (n,p)) neighbours 
--                              in  vs'++vs
--                    ) []
--                      ns
--    let g::Gr Pos Int = undir $ mkGraph
--                            (map (\n->(fromJust $ elemIndex n ns, n)) ns)
--                            (map (\(n1,n2)->(fromJust $ elemIndex n1 ns, fromJust $ elemIndex n2 ns, 1)) vertices)
--    let (startLabel, destLabel) = ((17,12), (17,1))
--    let start::Node = fromJust $ elemIndex startLabel ns
--    let dest ::Node = fromJust $ elemIndex destLabel ns
--    let ln = labNodes g
--    let node lab = fst $ fromJust $ find (\(_,l)->l==lab) ln
--    let start'::Node = node startLabel
--    let dest' ::Node = node destLabel
--    print (start, dest)
--    -- print $ (map (lab g)) <$>  (sp start dest g)
--    print $ fromJust $ (map (fromJust . lab g)) <$> sp start dest g
--    print $ fromJust $ (map (fromJust . lab g)) <$> sp start' dest' g
--    return ()
--(Num n, Integral n)(Num n, Integral n)
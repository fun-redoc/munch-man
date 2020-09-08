{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
import Debug.Trace (trace)
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
    let !repa = toRepaDIM2 test
    let a1::StateT (PQ [] (Infinite Float) Pos, M.Map Pos (Maybe Pos,(Infinite Float)),GG (Infinite Float) Pos) IO [Pos] 
            = astar euclidian (1,1) (9,3)
    !astarPath <- evalStateT a1 
                             ( emptyPriorityQueue::PQ [] (Infinite Float) Pos
                             , M.empty::M.Map Pos (Maybe Pos,(Infinite Float))
                             , repa
                             )
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
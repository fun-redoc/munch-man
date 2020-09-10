{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module AStar (astar, Heuristics, euclidian) where

import Debug.Trace (trace)

import Graph as G 
import WGraph as WG
import PriorityQueue as PQ
import Control.Monad.Trans.State.Lazy (put, get, StateT)
import Data.Map as M
import Control.Monad.Loops
import Data.Maybe (fromMaybe, isNothing, fromJust)
import Data.Traversable (for)
import Control.Monad (join, forM_, forM)
import Data.Hashable (Hashable)
import Data.List (sortOn, nubBy)

type Heuristics g a n = Num n=>(Graph g a)=>g a->a->a->n

euclidian::(Integral n, Num n)=>Heuristics g (n, n) (Infinite Float)
euclidian g (x1,y1) (x2,y2) = sqrt' $ fromIntegral $ (dx*dx + dy*dy)
                               where dx = x2-x1
                                     dy = y2-y1
                                     sqrt' (Bound x) = Bound (sqrt x)
                                     sqrt' PositiveInfinity = PositiveInfinity
                                     sqrt' NegativeInfinity = undefined

backtrack::(Ord a, Show a, Eq a, Hashable a, Applicative t, Foldable t, Monoid (t a))=>a->a->M.Map a (Maybe a, n)->t a
backtrack start dest dist_mat = iterate dest (pure dest) 
    where iterate dest' res = 
            if (dest' ==  start) || (isNothing prev_vert)
                then res 
                else iterate (fromJust prev_vert) res' 
                where
                    prev_dist = dist_mat M.!? dest'
                    prev_vert = join $ fst <$> prev_dist
                    res' = (pure $ fromJust prev_vert) <> res

-- | 1 to 1 monadic translation of the imperative algorithm
astar:: (Monad m, PriorityQueue pq t1 (Infinite n) a, Num n, Show a,
      Show n, WGraph g (Infinite n) a, Ord a, Ord n,
      Hashable a, Monoid (t2 a),
      Foldable t2, Applicative t2) =>
     (g (Infinite n) a -> a -> a -> Infinite n)
     -> a
     -> a
     -> StateT
          (pq t1 (Infinite n) a, Map a (Maybe a, Infinite n),
           g (Infinite n) a)
          m
          (t2 a)
astar heuristics startVertex destVertex = 
    do
        (open, closed, graph) <- get
        let open' = insert_with_priority open (startVertex, PositiveInfinity)
        let closed' = M.insert startVertex (Nothing, Bound 0) closed
        put (open', closed', graph)
        whileM_ (do (open,_,_) <- get
                    return $ not $ is_empty open
                )
                (do 
                    (open, closed, graph) <- get
                    let (cur, open') = pull_highest_priority_element open
                    put (open', closed, graph)
                    if isNothing cur
                        then return ()
                        else do
                            let cur' = fst $ fromJust cur
                            let neighboursWithWeight = WG.adjacent_vertices graph cur'
                            forM_ neighboursWithWeight 
                                (\(next, costToNext) -> do
                                    (open, closed, graph) <- get
                                    let !costSoFar = snd <$> closed M.!? cur'
                                    let !newCost = (+) <$> costSoFar <*> (Just costToNext)
                                    let !oldCost  = snd <$> (closed M.!? next)
                                    let !newCostIsLowerThanOldCost = (<) <$> newCost <*> oldCost
                                    if fromMaybe True newCostIsLowerThanOldCost
                                        then do
                                            let newCost' = fromJust newCost
                                            let !closed' = M.alter (const $ Just (Just cur', newCost')) next closed
                                            let !prio = newCost' + (heuristics graph destVertex next)
                                            let !open' = insert_with_priority open (next, prio)
                                            put (open', closed', graph)
                                            return ()
                                        else return ()
                                )
                            return ()
                )

        (open, closed, _) <- get
        return (backtrack startVertex destVertex closed) -- TODO handle case when dest or start is outside of the board/graph
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module AStar (astar, Heuristics, euclidian, RPQ) where

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

data RPQ t p a = RPQ (t (a,p)) 
instance (Show a, Eq a,  Ord p)=>PriorityQueue RPQ [] p a where
    emptyPriorityQueue = RPQ []
    is_empty (RPQ pq) = Prelude.null pq
    insert_with_priority (RPQ pq) (e,p) = RPQ $ nubBy (\(x,_) (y,_) -> if x /= y 
                                                                 then False 
                                                                 else True)
                                            $ reverse 
                                            $ sortOn snd ((e,p):pq)
    pull_highest_priority_element pq@(RPQ [])= (Nothing, pq)
    pull_highest_priority_element (RPQ (x:xs))= (Just x, RPQ xs)
    update_weight (RPQ xs) x p = RPQ $ sortOn snd $ Prelude.map (\(x',p') -> if x == x' then (x,p) else (x', p')) xs
instance (Show p, Show a)=>Show (RPQ [] p a) where
    show (RPQ pq) = show pq

type Heuristics g a n = Num n=>(Graph g a)=>g a->a->a->n

euclidian::Heuristics g (Float, Float) Float
euclidian g (x1,y1) (x2,y2) = sqrt (dx*dx + dy*dy)
                                where dx = x2-x1
                                      dy = y2-y1

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
--astar::(Show (pq t (Infinite n) a), Show n, Hashable a, Show a, Bounded n, Num n, Ord a, Eq a, WGraph g n a, Graph (g n) a, PriorityQueue pq t (Infinite n) a, Monad m)
--     -- Heuristics (g a) a n -> a -> a 
--     =>(g n a->a->a->(Infinite n))->a->a 
--     ->StateT (pq t (Infinite n) a, Map a (Maybe a, (Infinite n)), g n a) m [a]
astar heuristics startVertex destVertex = 
    do
        (open, closed, graph) <- get
        let open' = insert_with_priority open (startVertex, PositiveInfinity)
        let closed' = M.insert startVertex (Nothing, Bound 0) closed
        put (open', closed', graph)
        whileM_ (do (open,_,_) <- get
                    --let (cur,_) = pull_highest_priority_element open
                    --return $ maybe False (/=destVertex) $ fst <$> cur
                    return -- $ (trace (show ("while", open))) 
                           $ not $ is_empty open
                )
                (do 
                    (open, closed, graph) <- get
                    let (cur, open') = pull_highest_priority_element open
                    put (open', closed, graph)
                    if maybe False (/=destVertex) $ fst <$> cur
                        then do (trace (show ("go on ", cur))) return ()
                        else do (trace (show ("end", cur))) return ()
                    if isNothing cur
                        then return ()
                        else do
                            let cur' = fst $ fromJust cur
                            let neighboursWithWeight = WG.adjacent_vertices graph cur'
                            forM_ neighboursWithWeight 
                                (\(next, costToNext') -> do  -- use second arg as costToNext
                                    (open, closed, graph) <- get
                                    let !costSoFar = snd <$> closed M.!? cur'
                                    let !costToNext = get_weight graph cur' next
                                    let !newCost = (+) <$> costSoFar <*> (Just costToNext)
                                    let !oldCost  = snd <$> (closed M.!? next)
                                    let !newCostIsLowerThanOldCost = (<) <$> newCost <*> oldCost
                                    if fromMaybe True newCostIsLowerThanOldCost
                                        then do
--                                    if (not $ next `M.member` closed) || (newCost < (snd $ closed M.! next))
--                                        then do 
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
        return $ (trace (show ("backtrack", closed))) $ (backtrack startVertex destVertex closed)
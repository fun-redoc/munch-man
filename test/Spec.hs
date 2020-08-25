{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
import Graph
import Lib
import Board
import Data.Graph.Inductive.Graph (Node, mkGraph, lab, labNodes)
import Data.Graph.Inductive.PatriciaTree (Gr, UGr)
import Data.Graph.Inductive.Query.SP (sp, spLength, dijkstra)
import Data.List (elemIndex, nub, sort,find)
import Data.Maybe (isJust, fromJust)
import qualified Data.Array.Repa as R
import Data.Graph.Inductive (undir)



main :: IO ()
main = do
    let !repa = toRepaDIM2 board
    let ns::[Pos] = map snd $ map fromJust $ filter isJust (R.toList (unGG repa))
    let vertices = nub 
                 $ foldr 
                    (\n vs -> let neighbours = map snd $ adjacent_vertices_GG repa n
                                  vs' = sort $ map (\p -> (n,p)) neighbours 
                              in  vs'++vs
                    ) []
                      ns
    let g::Gr Pos Int = undir $ mkGraph
                            (map (\n->(fromJust $ elemIndex n ns, n)) ns)
                            (map (\(n1,n2)->(fromJust $ elemIndex n1 ns, fromJust $ elemIndex n2 ns, 1)) vertices)
    let (startLabel, destLabel) = ((17,12), (17,1))
    let start::Node = fromJust $ elemIndex startLabel ns
    let dest ::Node = fromJust $ elemIndex destLabel ns
    let ln = labNodes g
    let node lab = fst $ fromJust $ find (\(_,l)->l==lab) ln
    let start'::Node = node startLabel
    let dest' ::Node = node destLabel
    print (start, dest)
    -- print $ (map (lab g)) <$>  (sp start dest g)
    print $ fromJust $ (map (fromJust . lab g)) <$> sp start dest g
    print $ fromJust $ (map (fromJust . lab g)) <$> sp start' dest' g
    return ()

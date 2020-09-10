{-# LANGUAGE TemplateHaskell
           , DuplicateRecordFields
           , FlexibleInstances
           , RankNTypes
           , GADTs
           , MultiParamTypeClasses 
           , BangPatterns
           #-}
--{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# Language GADTs #-}
--{-# Language MultiParamTypeClasses #-}
--{-# Language FlexibleInstances #-}
--{-# Language FlexibleContexts #-}
--{-# Language ScopedTypeVariables #-}
--{-# Language RankNTypes #-}
module Board where

import Data.Maybe
import Debug.Trace
import Data.String
import Control.Lens
import Control.Lens.Operators
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as R
import qualified Data.Array.Repa.Shape as R

import Data.Array.Repa.Repr.Vector
import Graph
import Paths_munchman_gloss

import Lib
import Ghost
import GHC.Base (join)
import Data.List (elemIndex, sort, nub)
import Data.Graph.Inductive (Graph(mkGraph), undir)
import PriorityQueue

data GameField = GameField { _walls:: [RectEntity]
                           , _tunnels::[RectEntity]
                           , _ypills::[CircleEntity]
                           , _bpills::[CircleEntity]
                           , _ghosts::[RectEntity]
                           , _man::Maybe CircleEntity
                           , _path::[Vec]
                           , _repaPath::GG (Infinite Float)  Pos -- Array V DIM2 (Maybe Pos)
                           , _fieldGraph::FieldGraph
                           } 
makeLenses ''GameField
mkGameField::GameField
mkGameField = toGameField board

toGameField::[String]->GameField
toGameField xs = foldl (\gf (r,s) -> rowToGameField gf (rows-r) s) 
                       (GameField [] [] [] [] [] Nothing [] repa fieldGraph)
                 . (zip [1..]) 
                 $ xs
    where rows = fromIntegral $ length xs
          rowToGameField gf row = foldr (\(col,c) gf'->cellToGameField gf' row col c) gf . (zip [0..])
          cellToGameField gf row col c = 
              case c of
                  'X' -> gf&walls   <>~[(col,row,1,1)]
                  'O' -> gf&bpills  <>~[(col+0.5,row+0.5,0.3)]
                           &path    <>~[(col, row)]
                  '.' -> gf&ypills  <>~[(col+0.5,row+0.5,0.1)]
                           &path    <>~[(col, row)]
                  '=' -> gf&tunnels <>~[(col,row,1,1)]
                           &path    <>~[(col, row)]
                  'G' -> gf&ghosts  <>~[(col,row,1,1)]
                           &path    <>~[(col, row)]
                  '@' -> gf&man     .~ Just (col+0.5,row+0.5, 0.4)
                           &path    <>~[(col, row)]
                  '_' -> gf&path    <>~[(col, row)]
                  _ -> undefined
          !repa = toRepaDIM2 xs
          ns = map fst $ map fromJust $ filter isJust (R.toList (unGG repa))
          vertices = nub 
                   $ foldr 
                       (\n vs -> let neighbours = map fst $ adjacent_vertices_GG repa n
                                     vs' = sort $ map (\p -> (n,p)) neighbours 
                                 in  vs'++vs
                       ) []
                       ns
          fieldGraph = undir $ mkGraph
                            (map (\n->(fromJust $ elemIndex n ns, n)) ns)
                            (map (\(n1,n2)->(fromJust $ elemIndex n1 ns, fromJust $ elemIndex n2 ns, 1)) vertices)

test_board2::[String]
test_board2 = ["XXXXX"
              ,"X___X"
              ,"X___X"
              ,"X___X"
              ,"XXXXX"
              ]

test_board1::[String]
test_board1 = ["XXXXXXXXXXX"
              ,"X_________X"
              ,"X___XX____X"
              ,"X_________X"
              ,"XXXXXXXXXXX"
              ]

test_board::[String]
test_board = ["XXXX"
             ,"X__X"
             ,"X__X"
             ,"XXXX"
             ]

board::[String]
board = [ "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        , "X........X.......X.......X.........X"
        , "X.XX.XXX.X.XXXXX.X.XXXXX.X.XXXX.XX.X"
        , "XO................................OX"
        , "X.XX.X.XXXXX.X.XXXXX.X.XXXXXX.X.XX.X"
        , "X....X...X...X...X...X...X....X....X"
        , "XXXX.X.X.X.XXXXX.X.XXXXX.X.XX.X.XXXX"
        , "XXXX.X.X.....X.......X.....XX.X.XXXX"
        , "XXXX.X.X.XXX.X.XXGXX.X.XXX.XX.X.XXXX"
        , "=........X.....XGGGX.....X.........="
        , "XXXX.X.XXX.XXX.XXXXX.XXX.XXXX.X.XXXX"
        , "XXXX.X.......X.......X........X.XXXX"
        , "XXXX.X.XXXXX.X.XXXXX.X.XXXXXX.X.XXXX"
        , "XO.......X.......X.......X........OX"
        , "X.XX.XXX.X.XXXXX.X.XXXXX.X.XXXX.XX.X"
        , "X..X............................X..X"
        , "XX.X.X.XXXXX.X.XXXXX.X.XXXXXX.X.X.XX"
        , "X....X...X...X...X...X...X....X....X"
        , "X.XXXXXX.X.XXXXX.X.XXXXX.X.XXXXXXX.X"
        , "X................@.................X"
        , "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        ] 
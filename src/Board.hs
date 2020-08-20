{-# LANGUAGE TemplateHaskell
           , DuplicateRecordFields
           , FlexibleInstances
           , RankNTypes
           , GADTs
           , MultiParamTypeClasses 
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
import Data.Array.Repa
import Data.Array.Repa.Eval
import Data.Array.Repa.Shape
import Graph
import UGraph
import Paths_munchman_gloss

import Lib
import Ghost
import GHC.Base (join)

data GameField = GameField { _walls:: [RectEntity]
                           , _tunnels::[RectEntity]
                           , _ypills::[CircleEntity]
                           , _bpills::[CircleEntity]
                           , _ghosts::[RectEntity]
                           , _man::Maybe CircleEntity
                           , _path::[Vec]
                           , _repaPath::Array D DIM2 (Maybe Pos)
                           } deriving (Eq)
makeLenses ''GameField
mkGameField::GameField
mkGameField = toGameField board

toGameField::[String]->GameField
toGameField xs = foldl (\gf (r,s) -> rowToGameField gf (rows-r) s) 
                       (GameField [] [] [] [] [] Nothing [] (toRepaDIM2 xs))
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

toRepaDIM2::[String] -> Array D DIM2 (Maybe Pos)
toRepaDIM2 = Data.Array.Repa.map extract 
                 .fromListUnboxed (Z :. (35::Int) :. (20::Int))
                 .zip [(x,y)| y<-[0..19], x<-[0..34]]
                 .join 
    where extract ((x,y),c) = 
            if (c == 'O' || c == '.' || c == '_' || c == 'G' || c == '@')
                then Just (x,y)
                else Nothing


-- to lazy to implement all functions, only two really needed...
instance Graph (Array D DIM2) (Maybe Pos)  where
  emptyGraph                = undefined
  num_vertices              = undefined
  adjacent_vertices  g (Just (x,y)) = let [nCols, nRows] = listOfShape $ extent g
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
                                          leftNeighbour  = g ! (ix2 xLeft y)
                                          rightNeighbour = g ! (ix2 xRight y)
                                          upperNeighbour = g ! (ix2 x yUp)
                                          lowerNeighbour = g ! (ix2 x yDown)
                                      in filter isJust [leftNeighbour, rightNeighbour, upperNeighbour, lowerNeighbour]
  adjacent_vertices  _ Nothing  = []
  add_vertex                = undefined
  all_nodes                 =  filter isJust . toList

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
{-# LANGUAGE TemplateHaskell, DuplicateRecordFields, FlexibleInstances, RankNTypes, GADTs #-}
module Board where

import Data.String
import Control.Lens
import Control.Lens.Operators
import Paths_munchman_gloss

import Lib
import Ghost

data GameField = GameField { _walls:: [RectEntity]
                           , _tunnel::[RectEntity]
                           , _ypills::[CircleEntity]
                           , _bpills::[CircleEntity]
                           , _ghosts::[Ghost]
                           }
makeLenses ''GameField
mkGameField::GameField
mkGameField = GameField [] [] [] [] []

toGameField::[String]->GameField
toGameField = foldr (\(r,s) gf-> rowToGameField gf r s) mkGameField
              . (zip [0..]) 
    where rowToGameField gf row = foldr (\(col,c) gf'->cellToGameField gf' row col c) gf . (zip [0..])
          cellToGameField gf row col c = 
              case c of
                  'X' -> gf&walls  <>~[(row,col,1,1)]
                  'O' -> gf&bpills <>~[(row,col,0.2)]
                  '.' -> gf&ypills <>~[(row,col,0.1)]
                  '=' -> gf&tunnel <>~[(row,col,1,1)]
                  '_' -> gf
                  _ -> undefined

board::[String]
board = [ "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        , "X............XX..........XX..........XX............X"
        , "X.XXXX.XXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXX.XXXX.X"
        , "X.XXXX.XXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXX.XXXX.X"
        , "X.XXXX.XXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXX.XXXX.X"
        , "XO................................................OX"
        , "X.XXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXX.X"
        , "X.XXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXX.X"
        , "X......XX....XX....XX....XX....XX....XX....XX......X"
        , "XXXXXX.XX.XX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XX.XX.XXXXXX"
        , "XXXXXX.XX.XX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XX.XX.XXXXXX"
        , "XXXXXX.XX.XX.......XX..........XX.......XX.XX.XXXXXX"
        , "XXXXXX.XX.XX.XXXXX.XX.XXXX_XXX.XX.XXXXX.XX.XX.XXXXXX"
        , "XXXXXX.XX.XX.XXXXX.XX.XXXX_XXX.XX.XXXXX.XX.XX.XXXXXX"
        , "=............XX.......XXXX_XXX.......XX............="
        , "XXXXXX.XX.XXXXX.XXXXX.XXXXXXXX.XXXXX.XXXXX.XX.XXXXXX"
        , "XXXXXX.XX.XXXXX.XXXXX.XXXXXXXX.XXXXX.XXXXX.XX.XXXXXX"
        , "XXXXXX.XX..........XX..........XX..........XX.XXXXXX"
        , "XXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXX"
        , "XXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXX"
        , "XO...........XX..........XX..........XX...........OX"
        , "X.XXXX.XXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXX.XXXX.X"
        , "X.XXXX.XXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXX.XXXX.X"
        , "X...XX........................................XX...X"
        , "XXX.XX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XX.XXX"
        , "XXX.XX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XX.XXX"
        , "X......XX....XX....XX....XX....XX....XX....XX......X"
        , "X.XXXXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXXXX.X"
        , "X.XXXXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXXXX.X"
        , "X..................................................X"
        , "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        ] 


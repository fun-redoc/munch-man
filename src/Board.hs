{-# LANGUAGE TemplateHaskell, DuplicateRecordFields, FlexibleInstances, RankNTypes, GADTs #-}
module Board where

import Debug.Trace
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
                           , _man::Maybe CircleEntity
                           } deriving (Eq, Show)
makeLenses ''GameField
mkGameField::GameField
mkGameField = toGameField board --

toGameField::[String]->GameField
toGameField xs = foldl (\gf (r,s) -> rowToGameField gf (rows-r) s) (GameField [] [] [] [] [] Nothing)
                 . (zip [1..]) 
                 $ xs
    where rows = fromIntegral $ length xs
          rowToGameField gf row = foldr (\(col,c) gf'->cellToGameField gf' row col c) gf . (zip [0..])
          cellToGameField gf row col c = 
              case c of
                  'X' -> gf&walls  <>~[(col,row,1,1)]
                  'O' -> gf&bpills <>~[(col,row,0.2)]
                  '.' -> gf&ypills <>~[(col,row,0.1)]
                  '=' -> gf&tunnel <>~[(col,row,1,1)]
                  '@' -> gf&man .~ Just (col,row, 0.8)
                  '_' -> gf
                  _ -> undefined

board::[String]
board = [ "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        , "X........X.......X.......X.........X"
        , "X.XX.XXX.X.XXXXX.X.XXXXX.X.XXXX.XX.X"
        , "XO................................OX"
        , "X.XX.X.XXXXX.X.XXXXX.X.XXXXXX.X.XX.X"
        , "X....X...X...X...X...X...X....X....X"
        , "XXXX.X.X.X.XXXXX.X.XXXXX.X.XX.X.XXXX"
        , "XXXX.X.X.....X.......X.....XX.X.XXXX"
        , "XXXX.X.X.XXX.X.XX_XX.X.XXX.XX.X.XXXX"
        , "=........X.....X_@_X.....X.........="
        , "XXXX.X.XXX.XXX.XXXXX.XXX.XXXX.X.XXXX"
        , "XXXX.X.......X.......X........X.XXXX"
        , "XXXX.X.XXXXX.X.XXXXX.X.XXXXXX.X.XXXX"
        , "XO.......X.......X.......X........OX"
        , "X.XX.XXX.X.XXXXX.X.XXXXX.X.XXXX.XX.X"
        , "X..X............................X..X"
        , "XX.X.X.XXXXX.X.XXXXX.X.XXXXXX.X.X.XX"
        , "X....X...X...X...X...X...X....X....X"
        , "X.XXXXXX.X.XXXXX.X.XXXXX.X.XXXXXXX.X"
        , "X..................................X"
        , "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        ] 
--board = [ "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
--        , "X............XX..........XX..........XX............X"
--        , "X.XXXX.XXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXX.XXXX.X"
--        , "X.XXXX.XXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXX.XXXX.X"
--        , "X.XXXX.XXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXX.XXXX.X"
--        , "XO................................................OX"
--        , "X.XXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXX.X"
--        , "X.XXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXX.X"
--        , "X......XX....XX....XX....XX....XX....XX....XX......X"
--        , "XXXXXX.XX.XX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XX.XX.XXXXXX"
--        , "XXXXXX.XX.XX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XX.XX.XXXXXX"
--        , "XXXXXX.XX.XX.......XX..........XX.......XX.XX.XXXXXX"
--        , "XXXXXX.XX.XX.XXXXX.XX.XXXX_XXX.XX.XXXXX.XX.XX.XXXXXX"
--        , "XXXXXX.XX.XX.XXXXX.XX.XXXX_XXX.XX.XXXXX.XX.XX.XXXXXX"
--        , "=............XX.......XXXX_XXX.......XX............="
--        , "XXXXXX.XX.XXXXX.XXXXX.XXXXXXXX.XXXXX.XXXXX.XX.XXXXXX"
--        , "XXXXXX.XX.XXXXX.XXXXX.XXXXXXXX.XXXXX.XXXXX.XX.XXXXXX"
--        , "XXXXXX.XX..........XX..........XX..........XX.XXXXXX"
--        , "XXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXX"
--        , "XXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXX"
--        , "XO...........XX..........XX..........XX...........OX"
--        , "X.XXXX.XXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXX.XXXX.X"
--        , "X.XXXX.XXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXX.XXXX.X"
--        , "X...XX........................................XX...X"
--        , "XXX.XX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XX.XXX"
--        , "XXX.XX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XX.XXX"
--        , "X......XX....XX....XX....XX....XX....XX....XX......X"
--        , "X.XXXXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXXXX.X"
--        , "X.XXXXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXX.XX.XXXXXXXXXX.X"
--        , "X..................................................X"
--        , "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
--        ] 


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
                           , _tunnels::[RectEntity]
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
                  'O' -> gf&bpills <>~[(col+0.5,row+0.5,0.3)]
                  '.' -> gf&ypills <>~[(col+0.5,row+0.5,0.1)]
                  '=' -> gf&tunnels <>~[(col,row,1,1)]
                  '@' -> gf&man .~ Just (col+0.5,row+0.5, 0.4)
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
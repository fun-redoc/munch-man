{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Data.Maybe (isNothing)
import Data.List
import Control.Lens
import Paths_munchman_gloss
import System.Random
import qualified Data.Vector as V

someFunc::IO ()
someFunc = putStrLn "NOT IMPLEMENTED"

type Field2d a = [[a]]
type Pos = (Int,Int)
type Vec = (Float,Float)
type Position = Vec
type Velocity = Vec
type AbsVelocity = Float

data Direction = GoLeft | GoRight  | GoUp | GoDown | Stop Direction
data Man = Man {_speed::AbsVelocity, _manPos::Position, _dir::Direction}
makeLenses ''Man
data Ghost = Ghost {_ghostPos::Position, _vel::Velocity, _g::StdGen}
makeLenses ''Ghost

data GameField = GameField { _walls::[Pos], _ypills::[Pos], _bpills::[Pos], _man::Man, _ghosts::[Ghost]}
makeLenses ''GameField

data Game = Game { _level::Int, _field::GameField}
makeLenses ''Game

data GameState = StartGame --      Game
               | Playing           Game
               | WonGame           Game
               | LostGame          Game
               | FinishedAllLevels Game
               | ErrorState        String
makeLenses ''GameState

toDirVec::Direction->AbsVelocity->Velocity
toDirVec (Stop _) _    = (0,0)
toDirVec GoLeft speed  = (-speed, 0)
toDirVec GoRight speed = (speed, 0)
toDirVec GoUp speed    = (0, speed)
toDirVec GoDown speed  = (0, -speed)

add::(Num a)=>(a,a)->(a,a)->(a,a)
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

--mul::Point->Point->Point
--mul (x,y) (x',y') = (x*x',y*y')
--div::Point->Point->Point
--div (x,y) (x',y') = (x/x', y/y')

isWon::GameField->Bool
isWon _ = False

fileLevelReader ::  Int -> IO (Maybe GameField)
fileLevelReader n = do
  return $  Just (GameField [] [] [] (Man 2.0 (0,0) (Stop GoRight)) [])
--  allLevels <- allLevelsFileLevelReader
--  return $ allLevels V.!? n


{-# LANGUAGE TemplateHaskell, DuplicateRecordFields, FlexibleInstances, RankNTypes, GADTs #-}
module Game where

import Debug.Trace (trace)

import Data.Maybe (isNothing)
import Data.List
import Control.Lens
import Control.Monad.Trans.State.Strict
import Paths_munchman_gloss
import System.Random
import qualified Data.Vector as V

import Lib
import Event
import Man
import Pill
import Ghost
import Board


data Game = Game { _level::Int
                 , _field::GameField
                 , _manState::ManState
                 }
makeLenses ''Game

data GameScene = StartGame
               | Playing           Game
               | WonGame           Game
               | LostGame          Game
               | FinishedAllLevels
               | ErrorState        String
makeLenses ''GameScene
playScene::(Monad m)=>DeltaTime->GameEvent->GameScene->m GameScene
playScene dt evt s = case s of
                       StartGame         -> playSceneStartGame dt evt
                       (Playing game)    -> playScenePlaying dt evt game
                       (LostGame game)    -> undefined -- TODO
                       (WonGame game)    -> undefined -- TODO
                       FinishedAllLevels -> undefined -- TODO
                       (ErrorState _)    -> return s


playScenePlaying::(Monad m)=>DeltaTime->GameEvent->Game->m GameScene
playScenePlaying dt (GameEventManGo dir) game =
    return . either Playing LostGame-- Right = Scene change, Left = stay in scene
           . ghostCollision 
           -- start digesting pill, change man state
           . over (manState.Man.object) id -- TODO
           -- adjust man running against wall
           . over manState (handleWallsCollision game)
           -- adjust man running throu holes
           . over (manState.Man.object) id -- TODO
           -- preliminary move man
           . over manState (handlePreliminaryManMove dt dir) 
           -- move ghosts
           . over (field.ghosts) id  --TODO
           $ game
playScenePlaying dt GameEventNoOp game =
    return . either Playing LostGame -- Right = Scene change, Left = stay in scene
           . ghostCollision 
           -- start digesting pill, change man state
           -- TODO also think on blue pills
           . (\g -> let ypill' = find (hasPillCollision (g^.manState.Man.object)) (g^.field.ypills) 
                    in  maybe
                          g
                          (\yp -> case g^.manState.action of
                                    ManActionGo dir      -> g&field.ypills %~ (delete yp)
                                                             &manState.yellow +~ 1
                                                             &manState.action .~ ManActionEat (YellowPill yp) dir 0.1
                                    ManActionEat _ dir _ -> g&field.ypills %~ (delete yp)
                                                             &manState.yellow +~ 1
                                                             &manState.action .~ ManActionEat (YellowPill yp) dir 0.1
                                    ManActionStop  dir   -> g&field.ypills %~ (delete yp)
                                                             &manState.yellow +~ 1
                                                             &manState.action .~ ManActionEat (YellowPill yp) dir 0.1
                                    otherwise            -> (trace $ show (g^.manState)) $ undefined -- TODO
                          )
                          ypill' 
             )
           -- adjust man running against wall
           . over manState (handleWallsCollision game)
           -- adjust man running throu holes
           . over (manState.Man.object) id -- TODO
           -- preliminary move man
           . over manState (\s-> case s^.action of
                                  ManActionGo dir -> handlePreliminaryManMove dt dir s
                                  ManActionStop dir -> s
                                  ManActionEat  pill dir digestTime -> s&action    .~ (if digestTime <= 0 
                                                                                        then ManActionGo dir
                                                                                        else ManActionEat pill dir (digestTime - dt)
                                                                                      )
                                                                        &Man.object %~ (move dt dir (s^.Man.speed))
                                  ManActionGhostCollition ghost dir dyingTime -> undefined
                           )                                 
           -- move ghosts
           . over (field.ghosts) id  --TODO
           $ game
playScenePlaying dt GameEventManStop game = 
    return . either Playing LostGame-- Right = Scene change, Left = stay in scene
           . ghostCollision 
           -- preliminary move man
           . over manState (\s-> case s^.action of
                                        ManActionGo   dir -> s&action .~ ManActionStop dir
                                        ManActionStop dir -> s
                                        ManActionEat  pill dir digestTime -> s&action .~ ManActionStop dir
                                        ManActionGhostCollition ghost dir dyingTime -> undefined
                           )                                 
           -- move ghosts
           . over (field.ghosts) id  --TODO
           $ game
playScenePlaying _ e game = return $ ErrorState ("Unknow Event \"" ++ show e ++ "\" in Playing Scene.")


playSceneStartGame::(Monad m)=>DeltaTime->GameEvent->m GameScene
playSceneStartGame dt GameEventStartGame = return $ Playing mkGame
  where mkGame::Game
        mkGame    = Game 0 gameField manState
        gameField = mkGameField 
        manState  = mkManState (gameField^.man)
playSceneStartGame _ _  = return $ StartGame

-- | checks if there was a ghost collision
ghostCollision::Game->Either Game Game
ghostCollision game = -- TODO there is no collision detection yet
                      Left game -- default case, no collison

hasPillCollision::CircleEntity->CircleEntity->Bool
hasPillCollision c1@(x1,y1,r1) c2@(x2,y2,r2) =
  let distx = x1 - x2
      disty = y1 - y2
      distSqrd = distx*distx + disty*disty
  in  distSqrd < (r1+r2)*(r1+r2)

-- | check collision between a list of objects an a circle shape object
hasCollision::CircleEntity->RectEntity->Bool
hasCollision ci@(cx,cy,r) re@(rx,ry,w,h) = let cramp v mn mx = max mn (min v mx)
                                               closestx = cramp cx rx (rx+w)
                                               closesty = cramp cy ry (ry+h)
                                               distx    = cx - closestx
                                               disty    = cy - closesty
                                               distSqrd = distx*distx + disty*disty
                                           in  distSqrd < r*r
handlePreliminaryManMove dt dir  s = set lastState (Just s)
                                     . set action (ManActionGo dir)
                                     . over (Man.object) (move dt dir (s^.Man.speed)) 
                                     $ s
           
handleWallsCollision game s = if any (hasCollision (s^.Man.object)) (game^.field.walls) 
                          then maybe undefined id (s^.lastState)
                          else s

-- | check if game is won
-- TODO 
isWon::GameField->Bool
isWon _ = False

-- | read a level file, starting with 0
-- TODO
fileLevelReader ::  Int -> IO (Maybe GameField)
fileLevelReader n = do
  -- mock empty Field  
  return $  Just (GameField [] [] [] [] [] Nothing)
--  allLevels <- allLevelsFileLevelReader
--  return $ allLevels V.!? n


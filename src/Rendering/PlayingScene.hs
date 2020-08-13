{-# LANGUAGE BlockArguments, TemplateHaskell, BangPatterns, GADTs, ScopedTypeVariables #-}

module Rendering.PlayingScene where

import Debug.Trace (trace)

import Lib
import Man
import Event
import Step
import Pill
import Game
import Board
import World
import Rendering.Configuration

--import qualified Data.Vector as V
import Data.Either
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Data.List ( intercalate )
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State.Strict
import Codec.BMP
import Codec.Picture
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)
import System.IO.Error
import System.Environment
import System.Directory (getHomeDirectory)
import System.Exit
import Control.Lens
import Control.Lens.Operators
import qualified Sound.ALUT as AL hiding (Static)
import Paths_munchman_gloss

backgroundColor = makeColor 0 0 0 255
dirToTexture::GameConfiguraton->Dir->(Picture, Picture)
dirToTexture gameConf dir = case dir of
                                  DirLeft     -> (gameConf^.manL1,gameConf^.manL2)
                                  DirRight    -> (gameConf^.manR1,gameConf^.manR2)
                                  DirUp       -> (gameConf^.manU1,gameConf^.manU2)
                                  DirDown     -> (gameConf^.manD1,gameConf^.manD2)

manStateAsPicture::GameConfiguraton->ManState->Picture
manStateAsPicture gameConf manState = manTexture
  where
    manTexture = case manState^.action of
                      (ManActionGo dir)                             -> translate posx posy
                                                                       $ scale (r*gameConf^.scaleFactors._1) (r*gameConf^.scaleFactors._2) 
                                                                       $ fst $ dirToTexture gameConf dir
                      (ManActionStop dir)                           -> translate posx posy
                                                                       $ scale (r*gameConf^.scaleFactors._1) (r*gameConf^.scaleFactors._2)  
                                                                       $ fst $ dirToTexture gameConf dir
                      (ManActionEat  pill dir digestTime)           -> undefined -- Blank -- TODO
                      (ManActionGhostCollition ghost dir dyingTime) -> undefined -- Blank -- TODO
    (posx, posy, r) = (\(x,y,r) -> (x*gameConf^.factorX,y*gameConf^.factorY,r)) (manState^.object)

wallAsPicture::GameConfiguraton->RectEntity->Picture
wallAsPicture gameConf (x,y,w,h) =
    translate (x*gameConf^.factorX) (y*gameConf^.factorY) 
    $ (Color (greyN 0.6) $ rectangleSolid (w*(gameConf^.factorX)) (h*(gameConf^.factorY)))

wallsAsPicture::GameConfiguraton->[RectEntity]->[Picture]
wallsAsPicture gameConf objects = 
    map
        (wallAsPicture gameConf) 
        objects
    

playingSceneAsPicture::GameConfiguraton->Game->StateT World IO Picture
playingSceneAsPicture gameConf game = return $
  pictures ((wallsAsPicture gameConf (game^.field.walls))
           ++ [ manStateAsPicture gameConf (game^.manState) ]
           )

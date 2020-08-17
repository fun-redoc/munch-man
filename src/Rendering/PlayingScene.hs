{-# LANGUAGE BlockArguments, TemplateHaskell, BangPatterns, GADTs, ScopedTypeVariables #-}

module Rendering.PlayingScene where

import Debug.Trace (trace)

import Lib
import Man
import Ghost
import Event
import Step
import Pill
import Game
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
import Graphics.Gloss.Data.Color
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

ghostAsPicture::GameConfiguraton->Ghost->Picture
ghostAsPicture gameConf ghost = adjustToScale (gameConf^.ghostPic)
  where
    (posx, posy, w,h) = (\(x,y,w,h) -> (x*gameConf^.factorX,y*gameConf^.factorY,w,h)) (ghost^.Ghost.object)
    adjustToScale::Picture->Picture
    adjustToScale   =  translate posx posy . scale (w*gameConf^.ghostPicSize._1) (h*gameConf^.ghostPicSize._2) 

manStateAsPicture::GameConfiguraton->ManState->Picture
manStateAsPicture gameConf manState = manTexture
  where
    manTexture = case manState^.action of
                      (ManActionGo dir)                             -> adjustToScale
                                                                       $ fst $ dirToTexture gameConf dir
                      (ManActionStop dir)                           -> adjustToScale
                                                                       $ fst $ dirToTexture gameConf dir
                      (ManActionEat  pill dir digestTime)           -> adjustToScale
                                                                       $ snd $ dirToTexture gameConf dir
                      (ManActionGhostCollition ghost dir dyingTime) -> undefined -- Blank -- TODO
    (posx, posy, d) = (\(x,y,r) -> ((x-0.5)*gameConf^.factorX,(y-0.5)*gameConf^.factorY,r*2)) (manState^.Man.object)
    adjustToScale::Picture->Picture
    adjustToScale   =  translate posx posy . scale (d*gameConf^.scaleFactors._1) (d*gameConf^.scaleFactors._2) 

wallAsPicture::GameConfiguraton->RectEntity->Picture
wallAsPicture gameConf (x,y,w,h) =
    translate (x*gameConf^.factorX) (y*gameConf^.factorY) 
    $ (Color (greyN 0.6) $ rectangleSolid (w*(gameConf^.factorX)) (h*(gameConf^.factorY)))

wallsAsPicture::GameConfiguraton->[RectEntity]->[Picture]
wallsAsPicture gameConf objects = 
    map
        (wallAsPicture gameConf) 
        objects

pillAsPicture::GameConfiguraton->Color->CircleEntity->Picture
pillAsPicture gameConf c (x,y,r) =
    translate ((x-0.5)*gameConf^.factorX) ((y-0.5)*gameConf^.factorY) 
    $ (Color c $ circleSolid (r*(gameConf^.factorX)))

playingSceneAsPicture::GameConfiguraton->Game->StateT World IO Picture
playingSceneAsPicture gameConf game = return $
  pictures ((wallsAsPicture gameConf (game^.walls))
           ++ (map (\pill->pillAsPicture gameConf (pillColor pill) (pillCircleEntity pill)) (game^.pills) )
           ++ (map (ghostAsPicture gameConf) (game^.ghosts))
           ++ [ manStateAsPicture gameConf (game^.manState) ]
           )
    where pillColor (BluePill _) = blue
          pillColor (YellowPill _) = yellow

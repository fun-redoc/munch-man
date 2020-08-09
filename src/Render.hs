
{-# LANGUAGE BlockArguments, TemplateHaskell, BangPatterns, GADTs, ScopedTypeVariables #-}

module Render where

import Debug.Trace (trace)

import Lib
import Man
import Event
import Step
import Pill
import Game

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

type Size = Float
data AppMode = Ascii | Gloss deriving (Eq, Show, Read)
backgroundColor = makeColor 0 0 0 255
screenWidth  = 800::Float
screenHeight = 600::Float
fieldWidth::Float = 52
fieldHeight::Float = 31
factorX::Float = screenWidth/fieldWidth
factorY::Float = screenHeight/fieldHeight


data World  = World { _time::Float
                    , _event::GameEvent
                    , _scene::GameScene
                    }
makeLenses ''World

data GameConfiguraton = GameConfiguraton { _objectSize::Size
                                         , _scaleFactors::Point
                                         , _manR1::Picture
                                         , _manR2::Picture
                                         , _manL1::Picture
                                         , _manL2::Picture
                                         , _manU1::Picture
                                         , _manU2::Picture
                                         , _manD1::Picture
                                         , _manD2::Picture
                                         }
makeLenses ''GameConfiguraton


loadPicture::FilePath->IO (Picture, Point)
loadPicture fileName = do
   bmp <- getDataFileName fileName >>= readBMP
   when (isLeft bmp) $ do print (bmp^?!_Left)
                          exitFailure
   let texture = bitmapOfBMP (bmp^?!_Right)
   let size = bitmapSize $ bitmapDataOfBMP (bmp^?!_Right)
   return (texture, toPoint size)

loadPng::FilePath->IO (Picture, Point)
loadPng fileName = do
   png <- getDataFileName fileName >>= readPng
   when (isLeft png) $ do print (png^?!_Left)
                          exitFailure
   let maybeTexture = fromDynamicImage (png^?!_Right)
   when (isNothing maybeTexture) $ do print ("Wrong Format of image " ++ fileName)
                                      exitFailure
   let eitherSize = pngSize png
   when (isLeft eitherSize) $ do print (eitherSize^?!_Left)
                                 exitFailure
   return (fromJust maybeTexture, toPoint (eitherSize^?!_Right))
   where
   pngSize eitherPng = case eitherPng of
                          Right (ImageRGBA8 im)  -> Right (imageWidth im, imageHeight im)
                          otherwiae -> Left "unknow format"

toPoint::Pos -> Point
toPoint = over both fromIntegral
--
--mul::Point->Point->Point
--mul (x,y) (x',y') = (x*x',y*y')
--div::Point->Point->Point
--div (x,y) (x',y') = (x/x', y/y')

startSceneAsPicture::GameConfiguraton->Picture
startSceneAsPicture  gameConf = uncurry scale (0.3,0.3)
                               $ Color Graphics.Gloss.yellow
                               $ pictures [translate 0 120 $ Text "press some key"
                                          ,Text "to start game..."
                                          ]
errorSceneAsPicture gameConf errDesc = uncurry scale (0.15,0.15)
                                       $ Color Graphics.Gloss.red
                                       $ pictures [ translate 0 240 $ Text errDesc
                                                  , translate 0 120 $ Text "press some key"
                                                  , Text "to quit..."
                                                  ]
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
                                                                       $ fst $ dirToTexture gameConf dir
                      (ManActionStop dir)                           -> translate posx posy
                                                                       $ fst $ dirToTexture gameConf dir
                      (ManActionEat  pill dir digestTime)           -> Blank -- TODO
                      (ManActionGhostCollition ghost dir dyingTime) -> Blank -- TODO
    (posx, posy) = (\(x,y,r) -> (x*fieldWidth,y*fieldHeight)) (manState^.object)

playingSceneAsPicture::GameConfiguraton->Game->StateT World IO Picture
playingSceneAsPicture gameConf game = return $
  pictures [ manStateAsPicture gameConf (game^.manState)
           ]

gameAsPictureState::GameConfiguraton->StateT World IO Picture
gameAsPictureState gameConf = do world <- get
                                 let s = world^.scene
                                 case s of
                                  StartGame -> return $ startSceneAsPicture gameConf
                                  ErrorState desc -> return $ errorSceneAsPicture gameConf desc
                                  Playing game -> playingSceneAsPicture gameConf game
                                  _ -> return Blank
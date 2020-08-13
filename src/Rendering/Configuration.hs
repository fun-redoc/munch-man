{-# LANGUAGE BlockArguments, TemplateHaskell, BangPatterns, GADTs, ScopedTypeVariables #-}

module Rendering.Configuration where

import Debug.Trace (trace)

import Lib
import Man
import Event
import Step
import Pill
import Game
import World

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
backgroundColor = makeColor 0 0 0 255


data GameConfiguraton = GameConfiguraton { _objectSize::Size
                                         , _scaleFactors::Point
                                         , _screenWidth::Float
                                         , _screenHeight::Float
                                         , _fieldWidth::Float
                                         , _fieldHeight::Float
                                         , _factorX::Float
                                         , _factorY::Float
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

toPoint::Pos -> Point
toPoint = over both fromIntegral

mkConfiguration::IO GameConfiguraton
mkConfiguration = do
    -- load images
    (packManR1', packManR1Size) <- loadPng "PacManR1s.png"
    (packManR2', packManR2Size) <- loadPng "PacManR2s.png"
    let packManR1 = scale 1 1 packManR1'
    let packManR2 = scale 1 1 packManR2'
    (packManL1', packManL1Size) <- loadPng "PacManL1s.png"
    (packManL2', packManL2Size) <- loadPng "PacManL2s.png"
    let packManL1 = scale 1 1 packManL1'
    let packManL2 = scale 1 1 packManL2'
    (packManU1', packManU1Size) <- loadPng "PacManD1s.png"
    (packManU2', packManU2Size) <- loadPng "PacManD2s.png"
    let packManU1 = scale 1 1 packManU1'
    let packManU2 = scale 1 1 packManU2'
    (packManD1', packManD1Size) <- loadPng "PacManU1s.png"
    (packManD2', packManD2Size) <- loadPng "PacManU2s.png"
    let packManD1 = scale 1 1 packManD1'
    let packManD2 = scale 1 1 packManD2'

    let objectSize = uncurry max packManR1Size

    -- TODO replace configuration passing by Reader Monad
    let screenWidth  = 800::Float
    let screenHeight = 600::Float
    let fieldWidth::Float = 35
    let fieldHeight::Float = 20
    let factorX::Float = screenWidth/fieldWidth
    let factorY::Float = screenHeight/fieldHeight
    let manRadius = objectSize / 2
    let startPosX = screenWidth / 2
    let startPosY = screenHeight / 2
    let speed     = screenWidth / 10 -- 10 seconds to traverse the whole screen
    let (scalex, scaley) = (factorX/objectSize, factorY/objectSize)
    let scalePic = min scalex scaley
    let gameConf = GameConfiguraton objectSize
                                   (scalex, scaley)
                                   --(scalePic, scalePic)
                                   screenWidth
                                   screenHeight
                                   fieldWidth
                                   fieldHeight
                                   factorX
                                   factorY
                                   packManR1
                                   packManR2
                                   packManL1
                                   packManL2
                                   packManU1
                                   packManU2
                                   packManD1
                                   packManD2
    return gameConf



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
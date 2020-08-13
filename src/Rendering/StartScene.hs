{-# LANGUAGE BlockArguments, TemplateHaskell, BangPatterns, GADTs, ScopedTypeVariables #-}

module Rendering.StartScene where

import Debug.Trace (trace)

import Lib
import Man
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
import System.IO hiding (hGetContents)
import System.IO.Strict (hGetContents)
import System.IO.Error
import System.Environment
import System.Directory (getHomeDirectory)
import System.Exit
import Control.Lens
import Control.Lens.Operators
import Paths_munchman_gloss


startSceneAsPicture::GameConfiguraton->Picture
startSceneAsPicture  gameConf = uncurry scale (0.3,0.3)
                               $ Color Graphics.Gloss.yellow
                               $ pictures [translate 0 120 $ Text "press some key"
                                          ,Text "to start game..."
                                          ]
{-# LANGUAGE BlockArguments, TemplateHaskell, BangPatterns, GADTs, ScopedTypeVariables #-}

module Rendering.LostGameScene where

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
import qualified Sound.ALUT as AL hiding (Static)
import Paths_munchman_gloss

lostGameSceneAsPicture gameConf =   Color  (Graphics.Gloss.light $ Graphics.Gloss.red)
                                  $ translate 25 100
                                  $ uncurry scale (0.5,0.5)    
                                  $ pictures [ translate 300 450 $ Text "Game Over!"
                                             , translate 200 300 $ Text "press some key"
                                             , translate 0 150 $ Text "to play a new game..."
                                             ]
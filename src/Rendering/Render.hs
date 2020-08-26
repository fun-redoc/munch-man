{-# LANGUAGE BlockArguments, TemplateHaskell, BangPatterns, GADTs, ScopedTypeVariables #-}

module Rendering.Render where

import Debug.Trace (trace)

import Lib
import Man
import Event
import Step
import Pill
import Game
import World
import Rendering.Configuration
import Rendering.ErrorScene
import Rendering.StartScene
import Rendering.PlayingScene

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

gameAsPictureState::GameConfiguraton->StateT World IO Picture
gameAsPictureState gameConf = do  world <- get
                                  let s = world^.scene
                                  (translate (-0.5*(gameConf^.screenWidth-gameConf^.objectSize)) 
                                             (-0.5*(gameConf^.screenHeight-gameConf^.objectSize))) 
                                    <$> gamePic s
                              where hud w =  Color Graphics.Gloss.yellow
                                          $ translate 0 560
                                          $ uncurry scale (0.15,0.15)
                                          $ Text $ "Score: " ++ (show (Game.score (w^.scene)))
                                    gamePic s = case s of
                                                StartGame -> return $ startSceneAsPicture gameConf
                                                ErrorState desc -> return $ errorSceneAsPicture gameConf desc
                                                Playing game -> (mapStateT overlayHud 
                                                                $ playingSceneAsPicture gameConf game)
                                                _ ->  return Blank
                                    overlayHud::(IO (Picture, World) )-> (IO (Picture, World))
                                    overlayHud picM = picM >>= (\(pic, world) -> 
                                                                  return $ 
                                                                       (pictures [pic, hud world],world)
                                                               )

x gameConf = uncurry scale (0.3,0.3)
                               $ Color Graphics.Gloss.yellow
                               $ pictures [translate 0 120 $ Text "press some key"
                                          ,Text "to start game..."
                                          ]
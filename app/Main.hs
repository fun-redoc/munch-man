{-# LANGUAGE BlockArguments, TemplateHaskell, BangPatterns, GADTs, ScopedTypeVariables #-}

module Main where

import Debug.Trace (trace)

import Lib
import Man
import Event
import Step
import Pill
import Game
import World
import Rendering.Configuration
import Rendering.Render

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

data AppMode = Ascii | Gloss deriving (Eq, Show, Read)

-- | file path to store last played level number
levelPath::String
levelPath = "/.munchmanLastLevel.txt"

-- | load sound data
loadSound path = do
      -- Create an AL buffer from the given sound file.
      buf <- AL.createBuffer  (AL.File path)
      source <- AL.genObjectName
      AL.buffer source AL.$= Just buf
      return source

-- | play a single sound 
playSound :: AL.Source -> IO ()
playSound source = do
    AL.play [source]
    -- Normally nothing should go wrong above, but one never knows...
    errs <- AL.get AL.alErrors
    unless (null errs) $
        hPutStrLn stderr (intercalate "," [ d | AL.ALError _ d <- errs ])
    return ()

-- | play music loop
playLoop :: AL.Source -> IO ()
playLoop source = do
    AL.loopingMode source AL.$= AL.Looping
    playSound source

-- | retrieves the last played game level
getLastLevel::IO Int
getLastLevel = do
    res <- tryIOError $ do
                          homeDir <- getHomeDirectory
                          handle <- openFile (homeDir++levelPath) ReadMode
                          !contents <- hGetContents handle
                          hClose handle
                          return (read contents::Int)
    return $ fromRight 0 res

-- | saves the current level reached in the game
-- TODO should not be used in mai thread
putLevel::Int->IO ()
putLevel n = do
    res <- tryIOError $ do
                          homeDir <- getHomeDirectory
                          handle <- openFile (homeDir++levelPath) WriteMode
                          hPutStr handle (show n)
                          hClose handle
    return ()

-- | input handler wrapper allowing state and IO
handleInputIOState::Event                 -- ^ event to handle
                  ->StateT World IO ()    -- ^ current state of the game world
handleInputIOState evt = do world <- get
                            handleInput (world^.scene) evt
  where
    handleInput::(Monad m)=> GameScene->Event->StateT World m ()
    --handleInput _            (EventKey (Char 'q') Up _ _)   = event .= GameEventQuit
    handleInput StartGame    (EventKey (Char 'q') Up _ _) = event .= GameEventQuit
    handleInput StartGame    (EventKey _          Up _ _)   = event .= GameEventStartGame
    handleInput (LostGame _) (EventKey (Char 'q')  Up _ _)  = event .= GameEventQuit
    handleInput (LostGame _) (EventKey (Char  c )  Up _ _)  = event .= GameEventStartGame
    handleInput (Playing _)  (EventKey (Char  c ) Down _ _) =
      event .= case c of 'h' -> GameEventManGo DirLeft
                         'j' -> GameEventManGo DirUp
                         'k' -> GameEventManGo DirDown
                         'l' -> GameEventManGo DirRight
                         'a' -> GameEventManGo DirLeft
                         's' -> GameEventManGo DirUp
                         'w' -> GameEventManGo DirDown
                         'd' -> GameEventManGo DirRight
                         'q' -> GameEventQuit
                         _   -> GameEventNoOp
    handleInput (Playing _) (EventKey (Char c) Up _ _) =
      event .= case c of 'h' -> GameEventManStop
                         'j' -> GameEventManStop
                         'k' -> GameEventManStop
                         'l' -> GameEventManStop
                         'a' -> GameEventManStop
                         's' -> GameEventManStop
                         'w' -> GameEventManStop
                         'd' -> GameEventManStop
                         'q' -> GameEventQuit
                         _   -> GameEventNoOp
    handleInput _ _ = return ()


-- | the game loop with state
gameLoopIOState::Float                 -- ^ time passed since last loop in sec.
               -> StateT World IO ()   -- ^ the game state
gameLoopIOState dt = do time += dt
                        world <- get
                        if (world^.event) 0 == GameEventQuit 0
                          then liftIO exitSuccess
                          else do scene' <- playScene dt  ((world^.event) (world^.time)) (world^.scene)
                                  scene .= scene'
                                  event .= GameEventNoOp


main :: IO ()
main = AL.withProgNameAndArgs AL.runALUT $ \progName args -> do
  bluesLoop <- getDataFileName "BluesLoops_11_StayOnBeat.com.wav" >>= loadSound
  AL.sourceGain bluesLoop AL.$= 0.1 -- lower the volume of the loop
  playLoop bluesLoop

  args <- getArgs
  appMode <- case args of "ascii":[] -> return Ascii
                          "gloss":[] -> return Gloss
                          _          -> return Gloss
                          -- _ -> do putStrLn "Invalid arguments."
                          --          putStrLn "Usage:"
                          --          exitFailure
  let initialGame = World 0 GameEventNoOp StartGame
  gameConf <- mkConfiguration
  let window = InWindow ("Munchman "++show appMode) (round (gameConf^.screenWidth), round (gameConf^.screenHeight)) (100, 100)
  let backgroundColor = makeColor 0 0 0 255
  playIO window backgroundColor 30 initialGame
              (\w-> do rawPic <- evalStateT (gameAsPictureState gameConf) w
                       return $ rawPic
              )
              (execStateT.handleInputIOState)
              (execStateT . gameLoopIOState)
  exitSuccess

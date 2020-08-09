{-# LANGUAGE BlockArguments, TemplateHaskell, BangPatterns, GADTs, ScopedTypeVariables #-}

module Main where

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

-- | monadic lifting with 1 parameter
liftM'::Monad m=>(a->b)->(a->m b)
liftM' f x = return $ f x

-- | monadic lifting with 2 parameters
liftM''::Monad m=>(a->b->c)->(a->b->m c)
liftM'' f x y = return $ f x y

toPoint::Pos -> Point
toPoint = over both fromIntegral

mul::Point->Point->Point
mul (x,y) (x',y') = (x*x',y*y')
div::Point->Point->Point
div (x,y) (x',y') = (x/x', y/y')

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
    handleInput StartGame    (EventKey (Char 'q') Up _ _) = event .= GameEventQuit
    handleInput StartGame    (EventKey _          Up _ _) = event .= GameEventStartGame
    handleInput (Playing _)  (EventKey (Char c) Down _ _) =
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
gameLoopIOState dt = do world <- get
                        if world^.event == GameEventQuit
                          then liftIO exitSuccess
                          else do scene' <- playScene dt (world^.event) (world^.scene)
                                  scene .= scene'
                                  event .= GameEventNoOp

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

main :: IO ()
main = AL.withProgNameAndArgs AL.runALUT $ \progName args -> do
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
   --field' <- fileLevelReader 200
   --let initialGame = StartGame $ Game {_level=0, _field=field'}
   let manRadius = objectSize / 2
   let startPosX = screenWidth / 2
   let startPosY = screenHeight / 2
   let speed     = screenWidth / 10 -- 10 seconds to traverse the whole screen
   let initialGame = World 0 GameEventNoOp StartGame
   let initialManState = (ManState (ManActionStop DirRight) 0 0 speed (startPosX,startPosY,manRadius))
--   allLevels <- allLevelsFileLevelReader
--   let (xmax, ymax) = smul objectSize
--                    $ add (2,2)
--                    $ toPoint
--                    $ foldr (\(x,y) (x',y') -> (max x x', max y y')) (minBound, minBound) (V.foldr (\f a->a++(f^.walls)) [] allLevels)
--
--   let (scalex, scaley) = (screenWidth, screenHeight) `Main.div` (xmax, ymax)
--   let scalePic = min scalex scaley
   let (scalex, scaley) = (1, 1)
   let scalePic = min scalex scaley
   -- TODO replace configuration passing by Reader Monad
   let gameConf = GameConfiguraton objectSize
                                   (scalex, scaley)
                                   packManR1
                                   packManR2
                                   packManL1
                                   packManL2
                                   packManU1
                                   packManU2
                                   packManD1
                                   packManD2

   let window = InWindow ("Munchman "++show appMode) (round screenWidth, round screenHeight) (100, 100)
   playIO window backgroundColor 30 initialGame
               (\w-> do rawPic <- evalStateT (gameAsPictureState gameConf) w
                        return $ rawPic
               )
               (execStateT.handleInputIOState)
               (execStateT . gameLoopIOState)
   exitSuccess

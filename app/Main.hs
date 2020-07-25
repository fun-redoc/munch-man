{-# LANGUAGE BlockArguments, TemplateHaskell, BangPatterns #-}

module Main where

import Debug.Trace (trace)

import Lib
import qualified Data.Vector as V
import Data.Either
import Data.Maybe (fromMaybe, fromJust, isNothing)
import Data.List ( intercalate )
import Control.Monad
import Control.Monad.IO.Class (liftIO)
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
import qualified Sound.ALUT as AL hiding (Static)
import Paths_munchman_gloss

type Size = Float
data AppMode = Ascii | Gloss deriving (Eq, Show, Read)
backgroundColor = makeColor 0 0 0 255
screenWidth  = 640::Float
screenHeight = 400::Float

data World = World { _time::Float
                   , _spriteIdx::Int
                   , _state::GameState
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

levelPath::String
levelPath = "/.munchmanLastLevel.txt"

loadSound path = do
      -- Create an AL buffer from the given sound file.
      buf <- AL.createBuffer  (AL.File path)
      source <- AL.genObjectName
      AL.buffer source AL.$= Just buf
      return source

playSound :: AL.Source -> IO ()
playSound source = do
    AL.play [source]
    -- Normally nothing should go wrong above, but one never knows...
    errs <- AL.get AL.alErrors
    unless (null errs) $
        hPutStrLn stderr (intercalate "," [ d | AL.ALError _ d <- errs ])
    return ()

playLoop :: AL.Source -> IO ()
playLoop source = do
    AL.loopingMode source AL.$= AL.Looping
    playSound source

liftM'::Monad m=>(a->b)->(a->m b)
liftM' f x = return $ f x
liftM''::Monad m=>(a->b->c)->(a->b->m c)
liftM'' f x y = return $ f x y


getLastLevel::IO Int
getLastLevel = do
    res <- tryIOError $ do
                          homeDir <- getHomeDirectory
                          handle <- openFile (homeDir++levelPath) ReadMode
                          !contents <- hGetContents handle
                          hClose handle
                          return (read contents::Int)
    return $ fromRight 0 res

-- TODO should not be used in mai thread
putLevel::Int->IO ()
putLevel n = do
    res <- tryIOError $ do
                          homeDir <- getHomeDirectory
                          handle <- openFile (homeDir++levelPath) WriteMode
                          hPutStr handle (show n)
                          hClose handle
    return ()

-- TODO should not be used in mai thread
loadLevel::Int->IO GameState
loadLevel n = do level' <- fileLevelReader n
                 return $ case level' of
                            Nothing -> ErrorState "Failed to load Level."
                            Just level -> Playing $ Game n level

restartLevel game = loadLevel (game^.level)

handleInput::Event->GameState->GameState
handleInput (EventKey (Char c) Up   _ _) (Playing game) = Playing $ game & field . man . dir .~  (Stop (game^.field.man.dir))
handleInput (EventKey (Char c) Down _ _) (Playing game) = if isWon $ newGame^.field
                                                          then WonGame newGame
                                                          else Playing newGame
  where
    newGame = case c of
                   'a' -> game & field . man . dir .~  GoLeft
                   's' -> game & field . man . dir .~  GoDown
                   'w' -> game & field . man . dir .~  GoUp
                   'd' -> game & field . man . dir .~  GoRight
                   _   -> game
handleInput _                              gameState      = gameState

handleInputIO::Event->World->IO World
handleInputIO (EventKey (Char 'q') Down _ _)
              world@(World _ _ (Playing game))   = do putLevel (game^.level)
                                                      exitSuccess
handleInputIO (EventKey (Char 'r') Down _ _)
              world@(World _ _ (Playing game))   = do freshState <- restartLevel game
                                                      return $ world&state.~freshState
handleInputIO (EventKey (Char 'q') Down _ _) _   = exitSuccess
handleInputIO (EventKey _          Up   _ _)
              world@(World _ _ StartGame)        = do savedLevel <- getLastLevel
                                                      freshState <- loadLevel savedLevel
                                                      return $ world&state.~freshState
handleInputIO (EventKey (SpecialKey KeyEnter) Up _ _)
              world@(World _ _ (WonGame   game)) = do putLevel (game^.level)
                                                      freshStateNextLevel <- loadLevel (game^.level + 1)
                                                      return $ world&state.~freshStateNextLevel


handleInputIO (EventKey _          Up   _ _)
              world@(World _ _ (LostGame  game))  = undefined
handleInputIO evt
              world@(World _ _ gameState)         = return $ world&state.~ handleInput evt gameState

toPoint::Pos -> Point
toPoint = over both fromIntegral

mul::Point->Point->Point
mul (x,y) (x',y') = (x*x',y*y')
div::Point->Point->Point
div (x,y) (x',y') = (x/x', y/y')



gameAsPicture::GameConfiguraton->World->Picture
gameAsPicture gameConf
              (World _ _ (ErrorState desc)) = uncurry scale (gameConf^.scaleFactors)
                                            $ Color red
                                            $ pictures [ translate 0 (6*(gameConf^.objectSize)) $ Text desc
                                                       , translate 0 (3*(gameConf^.objectSize)) $ Text "press some key"
                                                       , Text "to quit..."
                                                       ]
gameAsPicture gameConf
              (World _ _ StartGame)         = uncurry scale (gameConf^.scaleFactors )
                                            $ Color yellow
                                            $ pictures [translate 0 (3*(gameConf^.objectSize)) $ Text "press some key"
                                                       ,Text "to start game..."
                                                       ]

gameAsPicture gameConf
              (World _ i (Playing game))    =
  pictures [
--             pictures $ fmap (translate_ (gameConf^.wallTexture) . toPoint) (game ^. (field . walls))
--           , pictures $ fmap (translate_ (gameConf^.cratePutTexture) . toPoint) (game ^. (field . storage))
----           , pictures $ fmap (translate_ (Color (greyN 0.6) 
----                                          $ rectangleSolid (gameConf^.objectSize) (gameConf^.objectSize)
----                                         ) . toPoint) 
----                                         (game ^. (field . storage))
--           , pictures $ fmap (translate_ (gameConf^.crateTexture) . toPoint) (game ^. (field . crates))
             if i `mod` 30 < 15 then (translate_ (fst manTexture) (game^.field.man.manPos))
                                else (translate_ (snd manTexture) (game^.field.man.manPos))
           ]
    where
    translate_ object (x,y) = translate x y object
    manTexture              = case game^.field.man.dir of
                                (Stop oldDir)    -> (oldManTexture1,oldManTexture1) where oldManTexture1 = fst (dirToTexture gameConf oldDir)
                                otherwise        -> dirToTexture gameConf (game^.field.man.dir)
    dirToTexture::GameConfiguraton->Direction->(Picture, Picture)
    dirToTexture gameConf dir = case dir of
                                  GoLeft     -> (gameConf^.manL1,gameConf^.manL2)
                                  GoRight    -> (gameConf^.manR1,gameConf^.manR2)
                                  GoUp       -> (gameConf^.manU1,gameConf^.manU2)
                                  GoDown     -> (gameConf^.manD1,gameConf^.manD2)
                                  (Stop _)   -> (gameConf^.manR1,gameConf^.manR1)

gameAsPicture gameConf
              (World _ _ (WonGame game))    = uncurry scale (gameConf^.scaleFactors)
                                            $ Color green
                                            $ pictures [ translate 0 (6*(gameConf^.objectSize)) $ Text ("you've won level " ++ show (game^.level + 1))
                                                       , translate 0 (3*(gameConf^.objectSize)) $ Text "press some key"
                                                       , Text "to enter next level..."
                                                       ]
gameAsPicture gameConf _                    = undefined

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




gameLoopIO::Float->World->IO World
gameLoopIO dt world@(World _ i (Playing game)) =
  return $ world & state.~ Playing newGame
                 & time +~ dt
                 & if i > 30 then spriteIdx .~ 0 else spriteIdx +~ 1
  where
    dirVec  = toDirVec (game^.field.man.dir) (game^.field.man.speed)
    newGame = game & field.man.manPos %~ add dirVec
gameLoopIO _ world  = return world

main :: IO ()
main = AL.withProgNameAndArgs AL.runALUT $ \progName args -> do
   -- load images
   (packManR1', packManR1Size) <- loadPng "PacManR1s.png"
   (packManR2', packManR2Size) <- loadPng "PacManR2s.png"
   let packManR1 = scale 2 2 packManR1'
   let packManR2 = scale 2 2 packManR2'
   (packManL1', packManL1Size) <- loadPng "PacManL1s.png"
   (packManL2', packManL2Size) <- loadPng "PacManL2s.png"
   let packManL1 = scale 2 2 packManL1'
   let packManL2 = scale 2 2 packManL2'
   (packManU1', packManU1Size) <- loadPng "PacManU1s.png"
   (packManU2', packManU2Size) <- loadPng "PacManU2s.png"
   let packManU1 = scale 2 2 packManU1'
   let packManU2 = scale 2 2 packManU2'
   (packManD1', packManD1Size) <- loadPng "PacManD1s.png"
   (packManD2', packManD2Size) <- loadPng "PacManD2s.png"
   let packManD1 = scale 2 2 packManD1'
   let packManD2 = scale 2 2 packManD2'

   let objectSize = 25 -- uncurry max packManR1Size
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
   let initialGame = World 0 0 StartGame
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

   let window = InWindow ("Sokoban "++show appMode) (round screenWidth, round screenHeight) (100, 100)
   playIO window backgroundColor 30 initialGame
               (liftM'  (translate (-(screenWidth/2)+(objectSize*scalePic))
                                   (-(screenHeight/2)+(objectSize*scalePic))
                                   . scale scalePic scalePic
                                   . gameAsPicture (GameConfiguraton objectSize
                                                                     (scalex, scaley)
                                                                     packManR1
                                                                     packManR2
                                                                     packManL1
                                                                     packManL2
                                                                     packManU1
                                                                     packManU2
                                                                     packManD1
                                                                     packManD2
                                                                     )
                                   ))
               --(liftM'' handleInput)
               --(liftM'' (const id))
               handleInputIO
               gameLoopIO
   exitSuccess

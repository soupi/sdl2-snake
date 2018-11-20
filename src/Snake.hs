{- | This is an example application of the sdl2 package.

We will implement the game 'Snake'.

This module is composed of the following parts:

1. Main Loop
Contains boilerplate code that is common to most games. Responsible for the lifecycle of the game.

2. Types and Constants
The model of the game. Defines the relevant data types for this game.

3. Events
Process the subset of events we care about.

4. Update
Updates the game every frame according to the logic of the game. This is where the fun stuff happens.

5. Render
This is where we convert the current state of the game to something we can display on screen.

-}

{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module Snake where

import Prelude hiding (head, init, tail)
import Data.Word (Word32)
import Data.List.NonEmpty (NonEmpty((:|)), head, init, tail, toList)
import Foreign.C.Types (CInt)
import Control.Monad (when, unless)
import Control.Concurrent (threadDelay)
import SDL.Vect (Point(P), V2(..), V4(..))
import SDL (($=))
import System.Random (StdGen, mkStdGen, randomR)
import qualified SDL

---------------
-- Main Loop --
---------------

run :: IO ()
run = do
  SDL.initialize [SDL.InitVideo]

  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "Snake"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
  SDL.showWindow window

  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
         { SDL.rendererType = SDL.AcceleratedRenderer
         , SDL.rendererTargetTexture = False
         }

  SDL.rendererDrawColor renderer $= V4 0 0 0 0 -- black background

  loop renderer initGameState

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit


loop :: SDL.Renderer -> GameState -> IO ()
loop renderer state = do
  -- measure ticks at the start
  start <- SDL.ticks

  -- the pattern
  events <- fetchEvents
  let state' = update events state
  render renderer state'

  -- measure ticks at the end and regulate FPS
  end <- SDL.ticks
  regulateFPS 60 start end

  -- decide whether to continue or not
  unless (eQuit events) (loop renderer state')

-- | Will wait until ticks pass
regulateFPS :: Word32 -> Word32 -> Word32 -> IO ()
regulateFPS fps start end
  | fps == 0 = pure ()
  | otherwise = do
    let
      ticksPerFrame = 1000 `div` fps
      interval = end - start
      gap = ticksPerFrame - interval
      delayFor
        | gap < ticksPerFrame =
          fromIntegral $ max 0 gap
        | otherwise =
          fromIntegral ticksPerFrame
    threadDelay $ delayFor * 1000 -- threadDelay works in microseconds

-------------------------
-- Types and Constants --
-------------------------

data GameState
  = GameState
  { sSnake :: NonEmpty (V2 CInt) -- we model the snake as a non-empty list of blocks
  , sDirection :: (Maybe Direction)
  , sStatus :: SnakeStatus
  , sFood :: Maybe (V2 CInt)
  , sMoveTimer :: Int -- we use timers to control when stuff should happen
  , sFoodTimer :: Int
  , sRandomGen :: StdGen -- this is used to generate new food at psuedo random locations
  }
  deriving Show

data Direction
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving Show

data SnakeStatus
  = Alive
  | Dead
  | CollidedWithTail
  | CollidedWithWall
  deriving (Show, Eq)

data MyEvents
  = MyEvents
  { eQuit :: Bool
  , eArrowUp :: Bool
  , eArrowDown :: Bool
  , eArrowLeft :: Bool
  , eArrowRight :: Bool
  }
  deriving Show

initGameState :: GameState
initGameState = GameState
  { sSnake = V2 (blockSize * 7) (blockSize * 7) :| []
  , sDirection = Just DirRight
  , sStatus = Alive
  , sFood = Just $ V2 (23 * blockSize) (14 * blockSize)
  , sMoveTimer = 13 -- the units are frames
  , sFoodTimer = 360 -- the units are frames
  , sRandomGen = mkStdGen 17
  }

getX :: V2 CInt -> CInt
getX (V2 locX _) = locX

getY :: V2 CInt -> CInt
getY (V2 _ locY) = locY

snakeBodyBlockSize :: V2 CInt
snakeBodyBlockSize = V2 blockSize blockSize

blockSize :: CInt
blockSize = 24

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (720, 576)


---------------------
-- Game Processing --
---------------------


------------
-- Events --
------------

-- | Will return a record with the relevant event states
fetchEvents :: IO MyEvents
fetchEvents = do
  events <- SDL.pollEvents
  isKeyPressed <- SDL.getKeyboardState
  pure $ MyEvents
    { eQuit = elem SDL.QuitEvent $ map SDL.eventPayload events
    , eArrowUp = isKeyPressed SDL.ScancodeUp
    , eArrowDown = isKeyPressed SDL.ScancodeDown
    , eArrowLeft = isKeyPressed SDL.ScancodeLeft
    , eArrowRight = isKeyPressed SDL.ScancodeRight
    }


------------
-- Update --
------------

-- | Will update the game each frame
update :: MyEvents -> GameState -> GameState
update events state
  | sStatus state == Dead = state
  | otherwise =
    collide . updateFood . moveAndEat . changeDir events $ state

changeDir :: MyEvents -> GameState -> GameState
changeDir events state
  | eArrowUp    events = state { sDirection = Just DirUp }
  | eArrowDown  events = state { sDirection = Just DirDown }
  | eArrowLeft  events = state { sDirection = Just DirLeft }
  | eArrowRight events = state { sDirection = Just DirRight }
  | otherwise          = state

moveAndEat :: GameState -> GameState
moveAndEat state
  | sMoveTimer state == 0 =
    state
      { sMoveTimer = sMoveTimer initGameState - min (sMoveTimer initGameState - 2) (length (sSnake state))
      , sSnake =
        newBlock (sDirection state) (head $ sSnake state)
        :| (if ate state then toList else init) (sSnake state)
      , sFood =
        if ate state
          then Nothing
          else sFood state
      , sFoodTimer =
        if ate state
          then 60
          else sFoodTimer state - 1
      }
  | otherwise = state
    { sMoveTimer = sMoveTimer state - 1
    , sFoodTimer = sFoodTimer state - 1
    }

ate :: GameState -> Bool
ate state = Just (head $ sSnake state) == sFood state

newBlock :: Maybe Direction -> V2 CInt -> V2 CInt
newBlock dir (V2 locX locY) = case dir of
  Just DirUp -> V2 locX (locY - getY snakeBodyBlockSize)
  Just DirDown -> V2 locX (locY + getY snakeBodyBlockSize)
  Just DirLeft -> V2 (locX - getX snakeBodyBlockSize) locY
  Just DirRight -> V2 (locX + getX snakeBodyBlockSize) locY
  Nothing -> V2 locX locY
  
updateFood :: GameState -> GameState
updateFood state
  | sFoodTimer state == 0 =
    let
      ((* blockSize) -> x, stdGen')  = randomR (4, div screenWidth  blockSize - 4) (sRandomGen state)
      ((* blockSize) -> y, stdGen'') = randomR (4, div screenHeight blockSize - 4) stdGen'
    in state
      { sFoodTimer = sFoodTimer initGameState
      , sFood = maybe (Just $ V2 x y) (const Nothing) $ sFood state 
      , sRandomGen = stdGen''
      }
  | otherwise =
    state

collide :: GameState -> GameState
collide state
  | sStatus state /= Alive =
    state
    { sStatus = Dead
    , sDirection = Nothing
    }
  | any (head (sSnake state) ==) (tail $ sSnake state) =
    state
    { sStatus = CollidedWithTail
    , sDirection = Nothing
    }
  | getX (head $ sSnake state) < 0
    || getX (head $ sSnake state) >= screenWidth
    || getY (head $ sSnake state) < 0
    || getY (head $ sSnake state) >= screenHeight
    = state
    { sStatus = CollidedWithWall
    , sDirection = Nothing
    }
  | otherwise = state


------------
-- Render --
------------

-- | Will render the game on screen
render :: SDL.Renderer -> GameState -> IO ()
render renderer state = do
  SDL.rendererDrawColor renderer $= V4 0 0 0 0
  SDL.clear renderer

  let
    drawBlock location =
      SDL.fillRect renderer $
        Just $ SDL.Rectangle (P location) snakeBodyBlockSize

  SDL.rendererDrawColor renderer $= V4 maxBound 0 0 maxBound
  mapM_ drawBlock $ sFood state

  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound
  mapM_ drawBlock $ sSnake state

  when (sStatus state == CollidedWithTail) $
    putStrLn "The snake collided with it's tail :("

  when (sStatus state == CollidedWithWall) $
    putStrLn "The snake collided with the wall :("

  SDL.present renderer


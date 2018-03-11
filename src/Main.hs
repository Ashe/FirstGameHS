{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

import Control.Monad
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import Data.List (foldl')
import SDL.Raw.Timer as SDL hiding (delay)
import Text.Pretty.Simple

import Animations

import Paths_FirstGameHS(getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

frameLimit :: Int
frameLimit = 60

data GameState
  = State {
      entities :: Guy
    , uselessIntForCompilerWarning :: Int
  }

-- This is our game world. It only consists of one lonely guy
-- who has a position and a velocity
data Guy
    = Guy
    { position :: Point V2 CDouble
    , velocity :: V2 CDouble
    } deriving (Show, Eq)

-- Our initial guy starts out with him roughly in the middle
initialGuy :: Guy
initialGuy =
    Guy
    { position = P $ V2 (fromIntegral screenWidth / 2) (fromIntegral $ screenHeight - 100)
    , velocity = V2 0 0
    }

initialState :: GameState
initialState = State initialGuy 0

jumpVelocity :: V2 CDouble
jumpVelocity = V2 0 (-2)

walkingSpeed :: V2 CDouble
walkingSpeed = V2 1 0

gravity :: V2 CDouble
gravity = V2 0 0.7

-- These simplify matching on a specific key code
pattern KeyPressed a <- (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed False (SDL.Keysym _ a _)))
pattern KeyReleased a <- (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released _ (SDL.Keysym _ a _)))

-- This processed input and modifies velocities of things in our world accordingly
-- and then returns the new world
processInput :: GameState -> SDL.EventPayload -> GameState
processInput state@(State oldGuy@(Guy _ curVel) _) (KeyPressed SDL.KeycodeUp) =
  state { entities = oldGuy {velocity = curVel * V2 1 0 + jumpVelocity}}
processInput state@(State oldGuy@(Guy _ curVel) _) (KeyPressed SDL.KeycodeLeft) =
  state { entities = oldGuy {velocity = negate walkingSpeed + curVel}}
processInput state@(State oldGuy@(Guy _ curVel) _) (KeyPressed SDL.KeycodeRight) =
  state { entities = oldGuy {velocity = walkingSpeed + curVel}}

processInput state@(State oldGuy@(Guy _ curVel) _) (KeyReleased SDL.KeycodeUp) =
  state { entities = oldGuy {velocity = curVel - jumpVelocity}}
processInput state@(State oldGuy@(Guy _ curVel) _) (KeyReleased SDL.KeycodeLeft) =
  state { entities = oldGuy {velocity = curVel - negate walkingSpeed}}
processInput state@(State oldGuy@(Guy _ curVel) _) (KeyReleased SDL.KeycodeRight) =
  state { entities = oldGuy {velocity = curVel - walkingSpeed}}

processInput s _ = s

updateWorld :: CDouble -> GameState -> GameState
updateWorld delta state@(State (Guy (P pos) vel) _) = 
  let (V2 newPosX newPosY) =  pos + (gravity + vel) * V2 delta delta
      fixedX = max 0 $ min newPosX (fromIntegral screenWidth - 50)
      fixedY = max 0 $ min (fromIntegral screenHeight - 100) newPosY
   in state {entities = Guy (P $ V2 fixedX fixedY) vel }

main :: IO ()
main = do

  -- Initialise SDL
  SDL.initialize [SDL.InitVideo]

  -- Create a window with the correct screensize and make it appear
  window <- SDL.createWindow "FirstGameHS"
    SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window

  -- Create a renderer for the window for rendering textures
  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedRenderer
        , SDL.rendererTargetTexture = False
        }

  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  -- Make a surface from file
  xOutSurface <- getDataFileName "Assets/foo.bmp" >>= SDL.loadBMP
  texture <- SDL.createTextureFromSurface renderer xOutSurface

  -- Free the surface as we have a texture now
  SDL.freeSurface xOutSurface

  let loop last state = do
        events <- SDL.pollEvents

        -- Need to calculate the time delta
        now <- SDL.getPerformanceCounter
        freq <- SDL.getPerformanceFrequency

        let delta = (fromIntegral now - fromIntegral last) * 1000 / fromIntegral freq
            payloads = map SDL.eventPayload events
            quit = SDL.QuitEvent `elem` payloads

        -- Update functions
        let worldAfterInput = foldl' processInput state payloads
            newState        = updateWorld delta worldAfterInput

        SDL.clear renderer

        -- Render functions
        SDL.copy renderer texture Nothing Nothing
        -- Draw our world(guy) as a white rectangle
        let drawColor = SDL.rendererDrawColor renderer
        drawColor $= V4 255 255 255 0
        SDL.fillRect renderer . Just $ SDL.Rectangle (truncate <$> position (entities newState)) (V2 50 100)

        -- My attempt at an FPS limit. I don't write games so it is possible this is incorrect
        let frameDelay = 1000 / fromIntegral frameLimit
        when (delta < frameDelay) $ SDL.delay (truncate $ frameDelay - delta)

        SDL.present renderer
        unless quit $ loop now newState

  now <- SDL.getPerformanceCounter
  loop now initialState

  SDL.destroyWindow window
  SDL.quit

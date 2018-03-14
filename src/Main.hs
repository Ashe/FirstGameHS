{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Main where

import Control.Monad
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import qualified SDL.Image
import Data.List (foldl')
import SDL.Raw.Timer as SDL hiding (delay)
import Text.Pretty.Simple

import SDLAnimations

import Paths_FirstGameHS(getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

frameLimit :: Int
frameLimit = 60

data GameState = 
  State
  { entities :: Guy
    , uselessIntForCompilerWarning :: Int
  }

-- This is our game world. It only consists of one lonely guy
-- who has a position and a velocity
data Guy = 
    Guy
    { position :: Point V2 CDouble
    , velocity :: V2 CDouble
    , tag :: String
    , animation :: String
    , frame :: Int
    } deriving (Show, Eq)

-- Our initial guy starts out with him roughly in the middle
initialGuy :: Guy
initialGuy =
    Guy
    { position = P $ V2 (fromIntegral screenWidth / 2) (fromIntegral $ screenHeight - 100)
    , velocity = V2 0 0
    , tag = "male"
    , animation = "idle"
    , frame = 0
    }

initialState :: GameState
initialState = State initialGuy 0

jumpVelocity :: V2 CDouble
jumpVelocity = V2 0 (-800)

walkingSpeed :: V2 CDouble
walkingSpeed = V2 300 0

gravity :: V2 CDouble
gravity = V2 0 300

-- These simplify matching on a specific key code
pattern KeyPressed a <- (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed False (SDL.Keysym _ a _)))
pattern KeyReleased a <- (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released _ (SDL.Keysym _ a _)))

-- This processed input and modifies velocities of things in our world accordingly
-- and then returns the new world
processInput :: GameState -> SDL.EventPayload -> GameState
processInput state@(State oldGuy@(Guy _ curVel _ _ _) _) (KeyPressed SDL.KeycodeUp) =
  state { entities = oldGuy {velocity = curVel * V2 1 0 + jumpVelocity}}
processInput state@(State oldGuy@(Guy _ curVel _ _ _) _) (KeyPressed SDL.KeycodeLeft) =
  state { entities = oldGuy {velocity = negate walkingSpeed + curVel}}
processInput state@(State oldGuy@(Guy _ curVel _ _ _) _) (KeyPressed SDL.KeycodeRight) =
  state { entities = oldGuy {velocity = walkingSpeed + curVel}}

processInput state@(State oldGuy@(Guy _ curVel _ _ _) _) (KeyReleased SDL.KeycodeUp) =
  state { entities = oldGuy {velocity = curVel - jumpVelocity}}
processInput state@(State oldGuy@(Guy _ curVel _ _ _) _) (KeyReleased SDL.KeycodeLeft) =
  state { entities = oldGuy {velocity = curVel - negate walkingSpeed}}
processInput state@(State oldGuy@(Guy _ curVel _ _ _) _) (KeyReleased SDL.KeycodeRight) =
  state { entities = oldGuy {velocity = curVel - walkingSpeed}}

processInput s _ = s

updateWorld :: CDouble -> GameState -> GameState
updateWorld delta state@(State (Guy (P pos) vel tag anim frame) _) = 
  let (V2 newPosX newPosY) =  pos + (gravity + vel) * V2 delta delta
      fixedX = max 0 $ min newPosX (fromIntegral screenWidth - 50)
      fixedY = max 0 $ min (fromIntegral screenHeight - 100) newPosY
   in state {entities = Guy (P $ V2 fixedX fixedY) vel tag anim frame }


-- Takes file and creates a texture out of it
getTextureFromImg :: SDL.Renderer -> FilePath -> IO SDL.Texture
getTextureFromImg renderer img = do
  surface <- SDL.Image.load =<< getDataFileName img
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  pure texture

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

  texture <- getTextureFromImg renderer "Assets/foo.bmp"
  player <- getTextureFromImg renderer "Assets/rogue.png"

  animsList <- loadAnimations "Assets/rogue.json"
  let animationSet = getAnimationSet "rogue" "male" =<< animsList
      animation = getAnimation "walk" =<< animationSet
      frame = fmap (getFrame 0) animation
      initAnimationState = 
        AnimationState animationSet animation [] "idle" 0 0

  let loop lastTicks state animState = do

        ticks <- SDL.getTicks
        events <- SDL.pollEvents

        let delta = 0.001 * (fromIntegral ticks - fromIntegral lastTicks)
            payloads = map SDL.eventPayload events
            quit = SDL.QuitEvent `elem` payloads

        -- Update functions
        let worldAfterInput = foldl' processInput state payloads
            newState        = updateWorld delta worldAfterInput
            newAnimState    = updateAnimationState delta 0.1 animState

        -- Render functions (Background and player)
        SDL.copy renderer texture Nothing Nothing
        SDL.copy renderer player (getCurrentFrame newAnimState) $ Just $ SDL.Rectangle (truncate <$> position (entities newState)) (V2 100 100)

        -- Delay time until next frame to save processing power
        let frameDelay = 1000 / fromIntegral frameLimit
        when (delta < frameDelay) $ SDL.delay (truncate $ frameDelay - delta)

        SDL.present renderer
        unless quit $ loop ticks newState newAnimState

  ticks <- SDL.getTicks
  loop ticks initialState initAnimationState

  SDL.destroyWindow window
  SDL.quit


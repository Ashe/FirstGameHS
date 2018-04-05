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

import Reactive.Banana
import Reactive.Banana.Frameworks

import GameState
import SDLAnimations
import InputModule

import Guy

import Paths_FirstGameHS(getDataFileName)

jumpVelocity :: V2 CDouble
jumpVelocity = V2 0 (-800)

walkingSpeed :: V2 CDouble
walkingSpeed = V2 300 0

gravity :: V2 CDouble
gravity = V2 0 300

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
  window <- SDL.createWindow "FirstGameHS" SDL.defaultWindow 
  let quitApplication = SDL.destroyWindow window >> SDL.quit

  -- Create a renderer for the window for rendering textures
  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedRenderer
        , SDL.rendererTargetTexture = False
        }

-- -- Set up the first state
-- let jump p@(Guy _ curVel _ _) = p { velocity = curVel * V2 1 0 + jumpVelocity }
--     fall p@(Guy _ curVel _ _) = p { velocity = curVel - jumpVelocity }
--     right p@(Guy _ curVel _ _) = p { velocity = walkingSpeed + curVel }
--     left p@(Guy _ curVel _ _) = p { velocity = curVel - walkingSpeed }
--
--     initOptions = initialOptions { keybindings = 
--       addBatchBindings 
--         [ ((SDL.KeycodeUp, True), Just (entManip 0 jump))
--         , ((SDL.KeycodeUp, False), Just (entManip 0 fall))
--         , ((SDL.KeycodeRight, True), Just (entManip 0 right))
--         , ((SDL.KeycodeRight, False), Just (entManip 0 left))
--         , ((SDL.KeycodeLeft, True), Just (entManip 0 left))
--         , ((SDL.KeycodeLeft, False), Just (entManip 0 right))
--         ] blankKeyBindings }
--     state = initialState { options = initOptions }


  -- Load in the background
  background <- getTextureFromImg renderer "Assets/foo.bmp"

  -- Load in the player's texture and animations
  animsList <- loadAnimations "Assets/rogue.json"
  playerTexture <- getTextureFromImg renderer "Assets/rogue.png"
  let animationSet = getAnimationSet "rogue" "male" =<< animsList
      animation = getAnimation "walk" =<< animationSet
      initAnimationState = 
        AnimationState animationSet animation [] "idle" 0 0

      -- Create the player and add it to the entitylist
      player = createGuy 0 0 playerTexture initAnimationState

  -- Create the initial state and put the player in
  let state = (initialState renderer) { entities = [player]}

  -- Set the window size
  SDL.windowSize window $= uncurry V2 (screenRes (options state))

  -- Show the window
  SDL.showWindow window

  ticks <- SDL.getTicks
  mainLoop state ticks background

  quitApplication

-- Main game loop
mainLoop :: Integral a => GameState -> a -> SDL.Texture -> IO ()
mainLoop state previousTicks bg = do
  ticks <- SDL.getTicks
  events <- SDL.pollEvents

  let delta = 0.001 * (fromIntegral ticks - fromIntegral previousTicks)
      payloads = map SDL.eventPayload events
      quit = SDL.QuitEvent `elem` payloads

  -- Update functions
  let newState = foldl' processInput state payloads

  -- Render functions (Background and player)
  SDL.copy (renderer newState) bg Nothing Nothing

  -- Render all entities
  renderGameState newState

  -- Delay time until next frame to save processing power
  let frameDelay = 1000 / fromIntegral (frameLimit (options newState))
  when (delta < frameDelay) $ SDL.delay (truncate $ frameDelay - delta)

  SDL.present $ renderer newState
  unless quit $ mainLoop newState ticks bg

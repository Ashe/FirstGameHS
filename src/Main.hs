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

  -- Set up the first state
-- let jump p@(Guy _ curVel _ _) = p { velocity = curVel * V2 1 0 + jumpVelocity }
--     fall p@(Guy _ curVel _ _) = p { velocity = curVel - jumpVelocity }
--     right p@(Guy _ curVel _ _) = p { velocity = walkingSpeed + curVel }
--     left p@(Guy _ curVel _ _) = p { velocity = curVel - walkingSpeed }
--
--     initOptions = initialOptions { keybindings = 
--       addBatchBindings 
--         [ ((SDL.KeycodeUp, True), Just (\s@(GameState _ es) -> s {entities = jump (head es) : tail es}))
--         , ((SDL.KeycodeUp, False), Just fall)
--         , ((SDL.KeycodeRight, True), Just right)
--         , ((SDL.KeycodeRight, False), Just left)
--         , ((SDL.KeycodeLeft, True), Just left)
--         , ((SDL.KeycodeLeft, False), Just right)
--         ] blankKeyBindings }
--     state = initialState { options = initOptions }

  let state = initialState

  -- Create a window with the correct screensize and make it appear
  window <- SDL.createWindow "FirstGameHS"
    SDL.defaultWindow { SDL.windowInitialSize = uncurry V2 (screenRes (options state)) }
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
      initAnimationState = 
        AnimationState animationSet animation [] "idle" 0 0

  let loop lastTicks state = do

        ticks <- SDL.getTicks
        events <- SDL.pollEvents

        let delta = 0.001 * (fromIntegral ticks - fromIntegral lastTicks)
            payloads = map SDL.eventPayload events
            quit = SDL.QuitEvent `elem` payloads

        -- Update functions
        let worldAfterInput = foldl' processInput state payloads
            newState        = updateGameState worldAfterInput delta

        -- Render functions (Background and player)
        SDL.copy renderer texture Nothing Nothing

        -- Delay time until next frame to save processing power
        let frameDelay = 1000 / fromIntegral (frameLimit (options newState))
        when (delta < frameDelay) $ SDL.delay (truncate $ frameDelay - delta)

        SDL.present renderer
        unless quit $ loop ticks newState

  ticks <- SDL.getTicks
  loop ticks state

  SDL.destroyWindow window
  SDL.quit


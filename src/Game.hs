-- The main loop of the game, binding everything
-- together from options to characters
------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

module Game
( game
, beginGame
) where

import System.Exit (exitSuccess)

import Control.Monad
import SDL.Vect
import SDL (($=))
import qualified SDL
import qualified SDL.Image

import Reflex
import Reflex.SDL2

import Common
import GameSetup
import SDLAnimations
import InputModule

import Guy

-- The main game loop
game :: (ReflexSDL2 r t m, MonadDynamicWriter t [Layer m] m) => GameSetup -> m () 
game setup = do

  -- When the network is finished setting up
  gameReady <- getPostBuild

  -- Set up time and limit values
  ticks <- getDeltaTickEvent
  limit <- holdDyn (frameLimit $ options setup) never
  unfTime <- foldDyn updateTime (createTime (frameLimit $ options setup)) (attachPromptlyDyn limit ticks)

  -- Filter out non-game ticks
  delta <- holdDyn (createTime 0) (ffilter nextFrame (updated unfTime))

  -- Create the player
  animsList <- loadAnimations "Assets/rogue.json"
  pTex <- getTextureFromImg (renderer setup) "Assets/rogue.png"
  let animationSet = getAnimationSet "rogue" "male" =<< animsList
      animation = getAnimation "walk" =<< animationSet
      pAnimState = 
        AnimationState animationSet animation [] "idle" 0 0

  -- Enter the recursive do block, to allow cyclic dependencies
  rec

    -- Set up the players
    player <- handleGuy state $ createGuy 0 0 pTex pAnimState
    player' <- handleGuy state $ createGuy 300 300 pTex pAnimState

    -- Every tick, render the background and all entities
    commitLayer $ ffor delta $ \_ -> SDL.copy (renderer setup) (texmex setup) Nothing Nothing
    commitLayer $ join $ ffor state $ \(State _ ps) -> renderEntities (renderGuy (renderer setup)) ps

    -- Create an initial state using data above
    let initialState =
          State
          { deltaTime = delta
          , ps = [player, player']
          }

    -- Create the state dynamic
    state <- holdDyn initialState never

  -- Quit on a quit event
  evQuit <- getQuitEvent
  performEvent_ $ ffor evQuit $ \() -> liftIO $ do
    SDL.quit
    SDL.destroyWindow $ window setup
    exitSuccess

-- Start the game loop properly
beginGame :: GameSetup -> IO ()
beginGame gs =
  host () $ do
    (_, dynLayers) <- runDynamicWriterT $ game gs
    performEvent_ $ ffor (updated dynLayers) $ \layers -> do
      rendererDrawColor r $= V4 0 0 0 255
      clear r
      sequence_ layers
      present r
  where w = window gs
        r = renderer gs

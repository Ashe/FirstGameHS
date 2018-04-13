-- The main loop of the game, binding everything
-- together from options to characters
------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Game
( game
, beginGame
) where

import System.Exit (exitSuccess)

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

  -- Create an event for every tick
  tmp <- getDeltaTickEvent
  delta <- holdDyn 0 (fmap ((/ 1000) .fromIntegral) tmp)

  -- Every tick sample the mouses current location
  mouseB <- hold (P $ V2 0 0) =<< performEvent (SDL.getAbsoluteMouseLocation <$ updated delta)

  -- Create the player
  animsList <- loadAnimations "Assets/rogue.json"
  pTex <- getTextureFromImg (renderer setup) "Assets/rogue.png"
  let animationSet = getAnimationSet "rogue" "male" =<< animsList
      animation = getAnimation "walk" =<< animationSet
      pAnimState = 
        AnimationState animationSet animation [] "idle" 0 0
  player <- handleGuy (updated delta) $ createGuy 0 0 pTex pAnimState
  
  -- Every tick, render the background and all entities
  commitLayer $ ffor delta $ \_ -> SDL.copy (renderer setup) (texmex setup) Nothing Nothing
  commitLayer $ ffor delta $ \_ -> renderGuy (current player) (renderer setup)
  -- commitLayer $ ffor delta $ \_ -> renderGameState gamestate

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

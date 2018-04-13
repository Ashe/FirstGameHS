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
game gamestate = do

  -- Create an event for every tick
  delta <- holdDyn 0 =<< getDeltaTickEvent

  -- Every tick sample the mouses current location
  mouseB <- hold (P $ V2 0 0) =<< performEvent (SDL.getAbsoluteMouseLocation <$ updated delta)
  
  -- Every tick, render the background and all entities
  commitLayer $ ffor delta $ \_ -> SDL.copy (renderer gamestate) (texmex gamestate) Nothing Nothing
  -- commitLayer $ ffor delta $ \_ -> renderGameState gamestate

  -- Quit on a quit event
  evQuit <- getQuitEvent
  performEvent_ $ ffor evQuit $ \() -> liftIO $ do
    SDL.quit
    SDL.destroyWindow $ window gamestate
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

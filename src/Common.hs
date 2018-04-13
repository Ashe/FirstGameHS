-- A module for miscellaneous functions and types
-- that may need to be used anywhere
-------------------------------------------------

{-# LANGUAGE FlexibleContexts      #-}

module Common
( Layer
, commitLayer
, commitLayers
, getTextureFromImg
) where

import qualified SDL
import qualified SDL.Image

import Reflex.SDL2

import Paths_FirstGameHS(getDataFileName)

-- A type representing one layer in our app.
type Layer m = Performable m ()

-- Commit a layer stack that changes over time.
commitLayers :: (ReflexSDL2 r t m, MonadDynamicWriter t [Layer m] m) => Dynamic t [Layer m] -> m ()
commitLayers = tellDyn

-- Commit one layer that changes over time.
commitLayer :: (ReflexSDL2 r t m, MonadDynamicWriter t [Layer m] m) => Dynamic t (Layer m) -> m ()
commitLayer = tellDyn . fmap pure

-- Takes file and creates a texture out of it
getTextureFromImg :: SDL.Renderer -> FilePath -> IO SDL.Texture
getTextureFromImg renderer img = do
  surface <- SDL.Image.load =<< getDataFileName img
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  pure texture

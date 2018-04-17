-- A module for miscellaneous functions and types
-- that may need to be used anywhere
-------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Common
( Layer
, Time (..)
, GameState (..)
, commitLayer
, commitLayers
, renderEntities
, getTextureFromImg
, updateTime
, createTime
) where

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import GHC.Word(Word32)
import Foreign.C.Types
import qualified SDL
import qualified SDL.Image

import Reflex
import Reflex.SDL2

import qualified Debug.Trace 

import Paths_FirstGameHS(getDataFileName)

-- A type representing one layer in the game
type Layer m = Performable m ()

-- Commit a layer stack that changes over time.
commitLayers :: (ReflexSDL2 r t m, MonadDynamicWriter t [Layer m] m) => Dynamic t [Layer m] -> m ()
commitLayers = tellDyn

-- Commit one layer that changes over time.
commitLayer :: (ReflexSDL2 r t m, MonadDynamicWriter t [Layer m] m) => Dynamic t (Layer m) -> m ()
commitLayer = tellDyn . fmap pure

-- Takes file and creates a texture out of it
getTextureFromImg :: MonadIO m => SDL.Renderer -> FilePath -> m SDL.Texture
getTextureFromImg renderer img = do
  surface <- liftIO $ SDL.Image.load =<< getDataFileName img
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  pure texture

-- Render multiple entities
renderEntities :: (Monad m, MonadIO mIO) => (a -> mIO ()) -> [m a] -> m (mIO ())
renderEntities f l = foldM (\_ g -> f g) () <$> sequence l

-- Data for containing time values
data Time =
  Time
  { delta     :: CDouble
  , elapsed   :: Word32
  , acc       :: Word32
  , limit     :: Word32
  , nextFrame :: Bool
  }

-- Easy way to create a Time
createTime :: Word32 -> Time
createTime limit = Time 0 0 0 limit True

-- Update the time with the time since previous frame
updateTime :: (Word32, Word32) -> Time -> Time
updateTime (lim, d) time =
  time
    { delta = fromIntegral (acc time) / 1000
    , elapsed = elapsed time + d
    , acc = fst check
    , limit = limit time
    , nextFrame = snd check
    }
    where ac = acc time + d
          check
            | limit time <= 0 = (0, True)
            | ac >= limit time = (mod ac (limit time), True)
            | otherwise = (ac, False)

-- An easy package containing lists for all entities and data
data GameState t p =
  State
  { deltaTime :: Dynamic t Time
  , ps :: [Dynamic t p] 
  }

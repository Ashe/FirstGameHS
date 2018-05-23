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
, renderSolidText
, renderBlendedText
, renderEntities
, getTextureFromImg
, getFontFromFile
, updateTime
, createTime
, getTime
) where

import Control.Monad
import Control.Arrow((***))
import Control.Applicative
import Control.Monad.IO.Class

import qualified Data.Text
import GHC.Word(Word32)
import Foreign.C.Types

import qualified SDL
import qualified SDL.Image
import qualified SDL.Font

import Reflex
import Reflex.SDL2

import qualified Debug.Trace 

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
getTextureFromImg renderer path = do
  liftIO $ putStrLn ("Loading image: " ++ show path) 
  surface <- SDL.Image.load path
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  pure texture

-- Load a font from a file
getFontFromFile :: MonadIO m => FilePath -> Int -> m SDL.Font.Font
getFontFromFile path size = do
  liftIO $ putStrLn ("Loading font: " ++ show path) 
  SDL.Font.load path size

-- Render text to the screen easily
renderText :: MonadIO m => SDL.Renderer -> SDL.Font.Font -> 
           (SDL.Font.Color -> Data.Text.Text -> m SDL.Surface) ->
           SDL.Font.Color -> String -> Int -> Int -> m ()
renderText r fo fu c t x y = do
  let text = Data.Text.pack t
  surface <- fu c text
  texture <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  size <- SDL.Font.size fo text
  let (w, h) = (fromIntegral *** fromIntegral) size
      x' = fromIntegral x
      y' = fromIntegral y
  SDL.copy r texture Nothing (Just (Rectangle (P $ V2 x' y') (V2 w h)))
  SDL.destroyTexture texture

-- Render solid text
renderSolidText :: MonadIO m => SDL.Renderer -> SDL.Font.Font -> 
                SDL.Font.Color -> String -> Int -> Int -> m ()
renderSolidText r fo = renderText r fo (SDL.Font.solid fo)

-- Render blended text
renderBlendedText :: MonadIO m => SDL.Renderer -> SDL.Font.Font -> 
                  SDL.Font.Color -> String -> Int -> Int -> m ()
renderBlendedText r fo = renderText r fo (SDL.Font.blended fo)

-- Render multiple entities
renderEntities :: MonadIO m => (a -> m ()) -> [a] -> m ()
renderEntities f = foldM (\_ e -> f e) ()

-- Data for containing time values
data Time =
  Time
  { delta       :: Double
  , elapsed     :: Word32
  , acc         :: Word32
  , frameLimit  :: Word32
  , nextFrame   :: Bool
  , postFrame   :: Bool
  }

-- Easy way to create a Time
createTime :: Word32 -> Time
createTime limit = Time 0 0 0 limit True False

-- Easy way to extract time from a gamestate
getTime :: Reflex t => Dynamic t (GameState t guy) -> Dynamic t Time
getTime s = join (deltaTime <$> s)

-- Update the time with the time since previous frame
updateTime :: (Word32, Word32) -> Time -> Time
updateTime (lim, d) time =
  time
    { delta = fromIntegral (acc time) / 1000
    , elapsed = elapsed time + d
    , acc = (\(a,_,_) -> a) check
    , frameLimit = lim
    , nextFrame = (\(_,a,_) -> a) check
    , postFrame = (\(_,_,a)->a) check
    }
    where ac = acc time + d
          limit
            | frameLimit time == 0 = 0
            | otherwise = round (1000 / fromIntegral (frameLimit time))
          check
            | limit <= 0 = (d, True, True)
            | postFrame time = (mod ac limit, False, False)
            | ac > limit = (ac, True, True)
            | otherwise = (ac, False, False)

-- An easy package containing lists for all entities and data
data GameState t guy =
  State
  { deltaTime :: Dynamic t Time
  , mouse :: Dynamic t SDL.MouseMotionEventData
  , ps :: [guy] 
  }

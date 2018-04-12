{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import Control.Monad
import Foreign.C.Types
import Data.List (foldl')
import SDL.Raw.Timer as SDL
import Text.Pretty.Simple
import Control.Concurrent (threadDelay)
import System.Exit (exitSuccess)

import SDL.Vect
import SDL (($=))
import qualified SDL
import qualified SDL.Image

import Reflex
import Reflex.SDL2

import GameState
import SDLAnimations
import InputModule

import Guy

import Paths_FirstGameHS(getDataFileName)

-- Takes file and creates a texture out of it
getTextureFromImg :: SDL.Renderer -> FilePath -> IO SDL.Texture
getTextureFromImg renderer img = do
  surface <- SDL.Image.load =<< getDataFileName img
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  pure texture

--------------------------------------------------------------------------------
-- | An axis aligned bounding box.
data AABB = AABB InputMotion (V2 Int)

--------------------------------------------------------------------------------
-- | Convert a mouse button to an AABB.
mouseButtonToAABB :: MouseButtonEventData -> AABB
mouseButtonToAABB dat = AABB (mouseButtonEventMotion dat) pos
  where P pos32 = mouseButtonEventPos dat
        pos = fromIntegral <$> pos32


--------------------------------------------------------------------------------
-- | Convert a mouse button motion to color.
motionToColor :: InputMotion -> V4 Int
motionToColor Released = V4 255 0 0   128
motionToColor Pressed  = V4 0   0 255 128


--------------------------------------------------------------------------------
-- | Renders an AABB using the handy SDL 2d 'Renderer'.
renderAABB :: MonadIO m => Renderer -> V4 Int -> V2 Int -> m ()
renderAABB r color pos = do
  rendererDrawColor r $= (fromIntegral <$> color)
  fillRect r $ Just $ Rectangle (P $ fromIntegral <$> pos - 10) 20


-------------------------------------------------------------------------------
-- | A type representing one layer in our app.
type Layer m = Performable m ()


----------------------------------------------------------------------
-- | Commit a layer stack that changes over time.
commitLayers :: (ReflexSDL2 r t m, MonadDynamicWriter t [Layer m] m) => Dynamic t [Layer m] -> m ()
commitLayers = tellDyn


----------------------------------------------------------------------
-- | Commit one layer that changes over time.
commitLayer :: (ReflexSDL2 r t m, MonadDynamicWriter t [Layer m] m) => Dynamic t (Layer m) -> m ()
commitLayer = tellDyn . fmap pure


-- The main game loop
game :: (ReflexSDL2 r t m, MonadDynamicWriter t [Layer m] m) => GameState -> m () 
game gamestate = do

  -- Create an event for every tick
  delta <- holdDyn 0 =<< getDeltaTickEvent
  mouseB <- hold (P $ V2 0 0) =<< performEvent (SDL.getAbsoluteMouseLocation <$ updated delta)
  
  -- Every tick, render the background and all entities
  commitLayer $ ffor delta $ \_ -> SDL.copy (renderer gamestate) (texmex gamestate) Nothing Nothing
  commitLayer $ ffor delta $ \_ -> renderGameState gamestate

  -- Quit on a quit event
  evQuit <- getQuitEvent
  performEvent_ $ ffor evQuit $ \() -> liftIO $ do
    SDL.quit
    SDL.destroyWindow $ window gamestate
    exitSuccess

main :: IO ()
main = do

  -- Initialise SDL
  SDL.initialize [SDL.InitVideo]

  -- Create a window with the correct screensize and make it appear
  window <- SDL.createWindow "FirstGameHS" SDL.defaultWindow 

  -- Create a renderer for the window for rendering textures
  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedRenderer
        , SDL.rendererTargetTexture = False
        }

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
  let state = (initialState window renderer background) { entities = [player]}

  -- Set the window size
  SDL.windowSize window $= uncurry V2 (screenRes (options state))

  -- Show the window
  SDL.showWindow window

  -- Begin the main game loop
  beginGame state

beginGame :: GameState -> IO ()
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

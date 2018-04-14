-- A character for the game
---------------------------

{-# LANGUAGE FlexibleContexts #-}

module Guy 
  ( Guy (..)
  , createGuy
  , updateGuy
  , renderGuy
  , renderGuys
  , handleGuy
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Foreign.C.Types
import SDL.Vect
import qualified SDL

import Reflex

import Common
import SDLAnimations

-- Simple character
data Guy =
 Guy
 { position   :: Point V2 CDouble
 , velocity   :: V2 CDouble
 , texture    :: SDL.Texture
 , animation  :: AnimationState
 }

-- Creates a guy out of simple data
createGuy :: CDouble -> CDouble -> SDL.Texture -> AnimationState -> Guy
createGuy x y tex anim =
  Guy
  { position  = P $ V2 x y
  , velocity  = V2 0 0
  , texture   = tex
  , animation = anim
  }

-- Update the guy's position and location
updateGuy :: Guy -> CDouble -> Guy
updateGuy guy dt = 
  guy
  { position =
    let (P pos) = position guy
        (V2 newPosX newPosY) = pos + velocity guy * V2 dt dt
     in P $ V2 newPosX newPosY
  , animation = updateAnimationState dt 0.1 (animation guy)
  }

-- Render multiple guys
renderGuys :: MonadIO m => SDL.Renderer -> [Guy] -> m ()
renderGuys r = foldM (\_ g -> renderGuy r g) ()

-- Render the guy
printGuy :: MonadIO m => SDL.Renderer -> Guy -> m ()
printGuy renderer g = liftIO . print $ position g

renderGuy :: MonadIO m => SDL.Renderer -> Guy -> m ()
renderGuy renderer g =
  SDL.copy renderer (texture g)
  (getCurrentFrame $ animation g) $ Just $ 
  SDL.Rectangle (truncate <$> position g) (V2 100 100)

handleGuy delta guy =
  foldDyn (flip updateGuy) guy delta

-- A character for the game
---------------------------

{-# LANGUAGE FlexibleContexts #-}

module Guy 
  ( Guy (..)
  , createGuy
  , updateGuy
  , renderGuy
  , handleGuy
  ) where

import Control.Monad.Fix
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

-- Print the guy to the terminal
printGuy :: MonadIO m => Guy -> m ()
printGuy g = liftIO . print $ position g

-- Render the guy
renderGuy :: MonadIO m => SDL.Renderer -> Guy -> m ()
renderGuy renderer g =
  SDL.copy renderer (texture g)
  (getCurrentFrame $ animation g) $ Just $ 
  SDL.Rectangle (truncate <$> position g) (V2 100 100)

-- Creates a dynamic from a guy
handleGuy :: (MonadFix m, MonadHold t m, Reflex t) => 
  Event t CDouble -> Guy -> m (Dynamic t Guy)
handleGuy delta guy = foldDyn (flip updateGuy) guy delta

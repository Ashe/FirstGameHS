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
 { position   :: Point V2 Double
 , velocity   :: V2 Double
 , texture    :: SDL.Texture
 , animation  :: AnimationState
 }

-- Creates a guy out of simple data
createGuy :: Double -> Double -> SDL.Texture -> AnimationState -> Guy
createGuy x y tex anim =
  Guy
  { position  = P $ V2 x y
  , velocity  = V2 0 0
  , texture   = tex
  , animation = anim
  }

-- Update the guy's position and location
updateGuy :: Guy -> Time -> Guy
updateGuy guy time@(Time dt _ _ _ _ _ _) = 
  guy
  { position =
    let (P pos) = position guy
        (V2 newPosX newPosY) = pos + velocity guy * V2 dt dt
     in P $ V2 newPosX newPosY
  , animation = updateAnimationState time (animation guy)
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
  Dynamic t (GameState t Guy) -> Guy -> m (Dynamic t Guy)
handleGuy st guy = foldDyn (flip updateGuy) guy delta
  where delta = switchDyn $ fmap (updated . deltaTime) st

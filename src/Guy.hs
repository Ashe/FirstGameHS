-- A character for the game
---------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Guy 
  ( Guy (..)
  , createGuy
  , renderGuy
  ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Foreign.C.Types
import SDL.Vect
import qualified SDL

import Reflex
import Reflex.SDL2

import Common
import SDLAnimations

-- Simple character
data Guy t =
 Guy
 { position   :: Dynamic t (Point V2 Double)
 , velocity   :: Behavior t (V2 Double)
 --, render     :: Behavior t (m ())
 , animation  :: Dynamic t AnimationState
 , texture    :: SDL.Texture
 }

-- Creates a guy out of simple data
createGuy :: (Reflex t, MonadHold t m, MonadFix m) => 
  Double -> Double -> SDL.Renderer -> Dynamic t Time -> SDL.Texture -> AnimationState -> m (Guy t)
createGuy x y r time tex anim = do
  vel <- hold (V2 0 0) never
  pos <- foldDyn updatePosition (P $ V2 x y) (attach vel $ updated time)
  animDyn <- foldDyn updateAnimationState anim $ updated time
  --let initRender = renderGuy r tex (P $ V2 x y, anim)
  --renderDyn <- hold initRender (renderGuy r tex <$> attach (current pos) (updated animDyn))
  pure Guy
    { position = pos
    , velocity = vel
    --, render = renderDyn
    , animation = animDyn
    , texture = tex
    }

-- Update the guy's position using velocity
updatePosition :: (V2 Double, Time) -> Point V2 Double -> Point V2 Double
updatePosition (vel, Time dt _ _ _ _ _) (P pos) = P $ V2 newPosX newPosY
  where (V2 newPosX newPosY) = pos + vel * V2 dt dt
  
-- Use the render function stored to produce an image on screen
renderGuy :: MonadIO m => SDL.Renderer -> SDL.Texture -> (Point V2 Double, AnimationState) -> m ()
renderGuy renderer t (p, a) =
  SDL.copy renderer t
    (getCurrentFrame a) $ Just $ 
    SDL.Rectangle (fmap truncate p) (V2 100 100)

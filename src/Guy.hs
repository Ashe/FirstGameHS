-- A character for the game
---------------------------

{-# LANGUAGE FlexibleContexts #-}

module Guy 
  ( Guy (..)
  , createGuy
  ) where

import Control.Monad
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
 , animation  :: Dynamic t AnimationState
 }

-- Easy way to refer to the requirements for rendering
type RenderReq = (SDL.Texture, Point V2 Double, AnimationState)

-- Creates a guy out of simple data
createGuy :: (ReflexSDL2 t m, MonadDynamicWriter t [Layer m] m) => 
  Dynamic t (GameState t guy) -> Double -> Double -> SDL.Renderer -> SDL.Texture -> AnimationState -> m (Guy t)
createGuy state x y r tex anim = do

  -- Define delta time
  let time = getTime state

  -- Set up dynamics and behaviors for guy
  vel <- hold (V2 0 0) never
  pos <- foldDyn updatePosition (P $ V2 x y) (attach vel $ updated time)
  animDyn <- foldDyn updateAnimationState anim $ updated time

  -- Render the guy
  let taggedEv = Reflex.tag (current animDyn) (updated state)
  render <- holdDyn (pure ()) (renderGuy r <$> attachWith (\a b -> (tex, a, b)) (current pos) taggedEv)
  commitLayer render

  -- Create the guy
  pure Guy
    { position = pos
    , velocity = vel
    , animation = animDyn
    }

-- Update the guy's position using velocity
updatePosition :: (V2 Double, Time) -> Point V2 Double -> Point V2 Double
updatePosition (vel, Time dt _ _ _ _ _) (P pos) = P $ V2 newPosX newPosY
  where (V2 newPosX newPosY) = pos + vel * V2 dt dt
  
-- Use the render function to produce render a guy on screen
renderGuy :: MonadIO m => SDL.Renderer -> RenderReq -> m ()
renderGuy renderer (t, p, a) = 
  SDL.copy renderer t (getCurrentFrame a) $ Just $ SDL.Rectangle (truncate <$> p) (V2 100 100)

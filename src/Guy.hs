-- A character for the game
---------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Guy 
  ( Guy (..)
  , createGuy
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
 , animation  :: Dynamic t AnimationState
 , texture    :: SDL.Texture
 }

-- Easy way to refer to the requirements for rendering
type RenderReq = (SDL.Texture, Point V2 Double, AnimationState)

-- Creates a guy out of simple data
createGuy :: (ReflexSDL2 r t m, MonadDynamicWriter t [Layer m] m) => 
  Event t a -> Double -> Double -> SDL.Renderer -> Dynamic t Time -> SDL.Texture -> AnimationState -> m (Guy t)
createGuy ev x y r time tex anim = do

  -- Set up dynamics and behaviors for guy
  vel <- hold (V2 0 0) never
  pos <- foldDyn updatePosition (P $ V2 x y) (attach vel $ updated time)
  animDyn <- foldDyn updateAnimationState anim $ updated time

  -- Render the guy
  let taggedEv = Reflex.tag (current animDyn) ev
  render <- holdDyn (pure ()) (renderGuy r <$> attachWith (\a b -> (tex, a, b)) (current pos) taggedEv)
  commitLayer render

  -- Create the guy
  pure Guy
    { position = pos
    , velocity = vel
    , animation = animDyn
    , texture = tex
    }

-- Update the guy's position using velocity
updatePosition :: (V2 Double, Time) -> Point V2 Double -> Point V2 Double
updatePosition (vel, Time dt _ _ _ _ _) (P pos) = P $ V2 newPosX newPosY
  where (V2 newPosX newPosY) = pos + vel * V2 dt dt
  
-- Use the render function to produce render a guy on screen
renderGuy :: MonadIO m => SDL.Renderer -> RenderReq -> m ()
renderGuy renderer (t, p, a) = 
  SDL.copy renderer t (getCurrentFrame a) $ Just $ SDL.Rectangle (truncate <$> p) (V2 100 100)

-- Get all the events required to render the guy
prepRender :: Reflex t => Guy t -> Event t RenderReq
prepRender g = attachPromptlyDynWith (\a b->(texture g,a,b)) (position g) (updated $ animation g)

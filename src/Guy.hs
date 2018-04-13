-- A character for the game
---------------------------

module Guy 
  ( Guy (..)
  , createGuy
  , updateGuy
  ) where

import Foreign.C.Types
import SDL.Vect
import qualified SDL

import Common
import SDLAnimations

 -- The fundamental structures of all our objects in the game
data Guy =
 Guy
 { position   :: Point V2 CDouble
 , velocity   :: V2 CDouble
 , texture    :: SDL.Texture
 , animation  :: AnimationState
 , render     :: SDL.Renderer -> IO ()
 }

-- Creates a guy out of simple data
createGuy :: CDouble -> CDouble -> SDL.Texture -> AnimationState -> Guy
createGuy x y tex anim =
  Guy
  { position  = P $ V2 x y
  , velocity  = V2 0 0
  , texture   = tex
  , animation = anim
  , render = renderGuy $ createGuy x y tex anim
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
  , render = renderGuy guy
  }

-- Render the guy
renderGuy :: Guy -> SDL.Renderer -> IO ()
renderGuy g renderer = 
  SDL.copy renderer (texture g) (getCurrentFrame (animation g)) $ Just $ SDL.Rectangle (truncate <$> position g) (V2 100 100)

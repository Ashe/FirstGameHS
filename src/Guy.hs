{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Guy 
  ( Guy(Guy)
  , position
  , velocity
  , texture
  , animation
  , guy
  , updateGuy
  ) where

import Control.Monad
import Foreign.C.Types
import SDL.Vect
import qualified SDL

import GameState
import SDLAnimations

 -- The fundamental structures of all our objects in the game
data Guy =
 Guy
 { position :: Point V2 CDouble
 , velocity :: V2 CDouble
 , texture  :: SDL.Texture
 , animation :: AnimationState
 }

guy :: Guy -> Int -> Entity
guy g i = 
  Entity
  { eID       = i
  , update    = flip guy i . updateGuy g
  , render    = renderGuy g
  }

updateGuy :: Guy -> GameState -> Guy
updateGuy g st = 
  g
  { position =
    let (P pos) = position g
        res = screenRes $ options $ st
        (V2 newPosX newPosY) = (pos + velocity g) * V2 dt dt
        fixedX = max 0 $ min newPosX (fromIntegral (fst res) - 50)
        fixedY = max 0 $ min (fromIntegral (snd res) - 100) newPosY
     in P $ V2 fixedX fixedY
  , animation = updateAnimationState dt 0.1 (animation g)
  }
  where dt = deltaTime st

renderGuy :: Guy -> SDL.Renderer -> IO ()
renderGuy g renderer = 
  SDL.copy renderer (texture g) (getCurrentFrame (animation g)) $ Just $ SDL.Rectangle (truncate <$> position g) (V2 100 100)
-- SDL.copy renderer player (getCurrentFrame newAnimState) $ Just $ SDL.Rectangle (truncate <$> position (entities newState)) (V2 100 100)

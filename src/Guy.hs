{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Guy 
  ( Guy(Guy)
  , position
  , velocity
  , tag
  , animation
  , guy
  , updateGuy
  ) where

import Control.Monad
import Foreign.C.Types
import SDL.Vect
import qualified SDL

import Entity
import SDLAnimations

 -- The fundamental structures of all our objects in the game
data Guy =
 Guy
 { position :: V2 CDouble
 , velocity :: V2 CDouble
 , tag :: String
 , animation :: AnimationState
 } deriving (Show)

guy :: Guy -> Int -> Entity
guy g i = 
  Entity
  { eID       = i
  , update    = flip guy i . updateGuy g
  , render    = print (i)
  }

updateGuy :: Guy -> CDouble -> Guy
updateGuy guy dt = 
  guy
  { position =
    let (V2 newPosX newPosY) = position guy + velocity guy * V2 dt dt
     in V2 newPosX newPosY
  , animation = updateAnimationState dt 0.1 (animation guy)
  }

-- -- Our initial guy starts out with him roughly in the middle
-- initialGuy :: (CDouble, CDouble) -> Entity
-- initialGuy pos =
--     Entity
--     { position = P $ V2 (fst pos) (snd pos)
--     , velocity = V2 0 0
--     , tag = "male"
--     , animation = "idle"
--     , frame = 0
--     }


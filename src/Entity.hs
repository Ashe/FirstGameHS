{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Entity 
  ( Entity(Entity)
  , position
  , velocity
  , tag
  , animation
  , frame
  ) where

import Control.Monad
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import qualified SDL.Image
import Data.List (foldl')
import SDL.Raw.Timer as SDL hiding (delay)
import Text.Pretty.Simple

-- The fundamental structures of all our objects in the game
data Entity =
  Entity
  { position :: Point V2 CDouble
  , velocity :: V2 CDouble
  , tag :: String
  , animation :: String
  , frame :: Int
  } deriving (Show, Eq)


-- Our initial guy starts out with him roughly in the middle
initialGuy :: (CDouble, CDouble) -> Entity
initialGuy pos =
    Entity
    { position = P $ V2 (fst pos) (snd pos)
    , velocity = V2 0 0
    , tag = "male"
    , animation = "idle"
    , frame = 0
    }


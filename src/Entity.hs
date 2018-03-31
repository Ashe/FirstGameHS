{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Entity 
  ( Entity(Entity)
  , eID
  , update
  , render
  ) where

import Control.Monad
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import qualified SDL.Image

data Entity =
  Entity
  { eID       :: Int
  , update    :: CDouble -> Entity
  , render    :: IO ()
  }

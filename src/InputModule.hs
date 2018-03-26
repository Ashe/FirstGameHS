-- This module is responsible for handling input flexibly

module InputModule ( KeyBindings(KeyBindings)
                   , getBoundInput
                   , blankKeyBindings
                   )
  where

import Control.Monad
import System.Directory
import Data.List
import Data.Monoid
import Data.Functor
import Data.Maybe
import Foreign.C.Types
import SDL
import qualified SDL

import qualified Data.Map as Map

-- State for keeping track of animations
-- newtype KeyBindings state = KeyBindings [(SDL.EventPayload, Maybe (state -> state))]
newtype KeyBindings state = KeyBindings (Map.Map SDL.EventPayload (Maybe (state -> state)))

-- Get a function out of keybindings
getBoundInput :: KeyBindings state -> SDL.EventPayload -> Maybe (state -> state)
getBoundInput (KeyBindings kbs) input = join $ Map.lookup input kbs

-- Add a function to a list of keybindings
addBinding :: SDL.EventPayload -> Maybe (state -> state) -> KeyBindings state -> KeyBindings state
addBinding bind func (KeyBindings map) = KeyBindings $ Map.insert bind func map

-- Blank keybindings map
blankKeyBindings :: KeyBindings state
blankKeyBindings = KeyBindings Map.empty

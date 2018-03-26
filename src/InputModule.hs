-- This module is responsible for handling input flexibly

module InputModule ( KeyBindings(KeyBindings)
                   , addBinding
                   , addBatchBindings
                   , getBoundInput
                   , blankKeyBindings
                   )
  where

import Control.Monad
import Foreign.C.Types
import SDL
import qualified SDL

import qualified Data.Map as Map

-- State for keeping track of animations
-- newtype KeyBindings state = KeyBindings [(SDL.Keycode, Maybe (state -> state))]
newtype KeyBindings state = KeyBindings (Map.Map (SDL.Keycode, Bool) (Maybe (state -> state)))

-- Blank keybindings map
blankKeyBindings :: KeyBindings state
blankKeyBindings = KeyBindings Map.empty

-- Get a function out of keybindings
getBoundInput :: KeyBindings state -> (SDL.Keycode, Bool) -> Maybe (state -> state)
getBoundInput (KeyBindings kbs) input = join $ Map.lookup input kbs

-- Set a function to a list of keybindings
addBinding :: (SDL.Keycode, Bool) -> Maybe (state -> state) -> KeyBindings state -> KeyBindings state
addBinding bind func (KeyBindings map) = KeyBindings $ Map.insert bind func map

-- Add a batch of functions to the keymappings (overwriting when necessary)
addBatchBindings :: [((SDL.Keycode, Bool), Maybe(state -> state))] -> KeyBindings state -> KeyBindings state
addBatchBindings [] kbs = kbs
addBatchBindings ((key, func):xs) kbs = addBatchBindings xs $ addBinding key func kbs

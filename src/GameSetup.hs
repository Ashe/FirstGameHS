-- A module for the storage and management of
-- settings, keybindings, resolution settings etc
-------------------------------------------------

module GameSetup  
  ( GameSetup (..)
  , Options (..)
  , initialSetup
  , initialOptions
  ) where

import GHC.Word(Word32)
import Foreign.C.Types
import qualified SDL

import InputModule

-- Hands the current state of the game to various functions
data GameSetup = 
  GameSetup
  { options     :: Options
  , window      :: SDL.Window
  , renderer    :: SDL.Renderer
  , texmex      :: SDL.Texture
  }

-- We will need this later so just making a newtype for now
data Options = 
  Options 
    { screenRes   :: (CInt, CInt)
    , frameLimit  :: Word32
    , keybindings :: KeyBindings GameSetup
    }

-- Create a GameSetup with some starting data easily
initialSetup :: SDL.Window -> SDL.Renderer -> SDL.Texture -> GameSetup
initialSetup = GameSetup initialOptions

-- Create a set of Options with default values
initialOptions :: Options
initialOptions =
  Options
    { screenRes = (640, 480)
    , frameLimit = 60
    , keybindings = blankKeyBindings :: KeyBindings GameSetup
    }

-- -- Change the gamestate with input
-- processInput :: GameSetup -> SDL.EventPayload -> GameState
-- processInput state@(State (Options _ _ kbs) _ _ _ _ _ _) input = 
--   exec $ join func
--   where func = getBoundInput kbs <$> processEvent input
--         exec (Just f) = f state
--         exec _ = state

-- -- Retrieves the keycode and press-status of key
-- processEvent :: SDL.EventPayload -> Maybe (SDL.Keycode, Bool)
-- processEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed False (SDL.Keysym _ code _))) = Just (code, True)
-- processEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released _ (SDL.Keysym _ code _))) = Just (code, False)
-- processEvent _ = Nothing
--
-- -- Renders the game state's entities
-- renderGameSetup :: Control.Monad.IO.Class.MonadIO m => GameState -> m ()
-- renderGameSetup state@(State _ _ _ _ _ r texMex) = liftIO $
--   SDL.copy r texMex Nothing Nothing
--     <* mapM (`render` r) (entities state)

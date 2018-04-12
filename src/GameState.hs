module GameState  
  ( GameState(State)
  , Options(Options)
  , KeyBindings
  , initialState
  , entities
  , renderer
  , options
  , deltaTime
  , elapsedTime
  , renderGameState
  , initialOptions
  , screenRes
  , frameLimit
  , keybindings
  , processInput
  , window
  , render
  , texmex
  ) where

import Control.Monad 
import Control.Monad.IO.Class
import Foreign.C.Types
import SDL
import qualified SDL

import InputModule
import Guy

-- Hands the current state of the game to various functions
data GameState = 
  State
  { options     :: Options
  , deltaTime   :: CDouble
  , elapsedTime :: CDouble
  , entities    :: [Guy]
  , window      :: SDL.Window
  , renderer    :: SDL.Renderer
  , texmex      :: SDL.Texture
  }

-- We will need this later so just making a newtype for now
data Options = 
  Options 
    { screenRes   :: (CInt, CInt)
    , frameLimit  :: Int
    , keybindings :: KeyBindings GameState
    }

initialState :: SDL.Window -> SDL.Renderer -> SDL.Texture -> GameState
initialState = State initialOptions 0 0 []

initialOptions :: Options
initialOptions =
  Options
    { screenRes = (1440, 1080)
    , frameLimit = 60
    , keybindings = blankKeyBindings :: KeyBindings GameState
    }

-- Change the gamestate with input
processInput :: GameState -> SDL.EventPayload -> GameState
processInput state@(State (Options _ _ kbs) _ _ _ _ _ _) input = exec $ join func
  where func = getBoundInput kbs <$> processEvent input
        exec (Just f) = f state
        exec _ = state

-- Retrieves the keycode and press-status of key
processEvent :: SDL.EventPayload -> Maybe (SDL.Keycode, Bool)
processEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed False (SDL.Keysym _ code _))) = Just (code, True)
processEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released _ (SDL.Keysym _ code _))) = Just (code, False)
processEvent _ = Nothing

-- Renders the game state's entities
renderGameState :: Control.Monad.IO.Class.MonadIO m => GameState -> m ()
renderGameState state@(State _ _ _ _ _ r texMex) = liftIO $
  SDL.copy r texMex Nothing Nothing
    <* mapM (`render` r) (entities state)

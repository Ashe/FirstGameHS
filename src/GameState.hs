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
  , render
  ) where

import Control.Monad
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
  , renderer    :: SDL.Renderer
  }

-- We will need this later so just making a newtype for now
data Options = 
  Options 
    { screenRes   :: (CInt, CInt)
    , frameLimit  :: Int
    , keybindings :: KeyBindings GameState
    }

initialState :: SDL.Renderer -> GameState
initialState r = State initialOptions 0 0 [] r

initialOptions :: Options
initialOptions =
  Options
    { screenRes = (1440, 1080)
    , frameLimit = 60
    , keybindings = blankKeyBindings :: KeyBindings GameState
    }

-- Change the gamestate with input
processInput :: GameState -> SDL.EventPayload -> GameState
processInput state@(State (Options _ _ kbs) _ _ _ _) input = exec $ join func
  where func = getBoundInput kbs <$> processEvent input
        exec (Just f) = f state
        exec _ = state

-- Retrieves the keycode and press-status of key
processEvent :: SDL.EventPayload -> Maybe (SDL.Keycode, Bool)
processEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed False (SDL.Keysym _ code _))) = Just (code, True)
processEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released _ (SDL.Keysym _ code _))) = Just (code, False)
processEvent _ = Nothing

-- Renders the game state's entities
renderGameState :: GameState -> IO [()]
renderGameState state@(State _ _ _ _ renderer) = sequence $ map (\g -> render g $ renderer) $ entities state

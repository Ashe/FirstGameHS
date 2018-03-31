module GameState  
  ( GameState(State)
  , Options(Options)
  , KeyBindings
  , initialState
  , entities
  , options
  , updateGameState
  , renderGameState
  , initialOptions
  , screenRes
  , frameLimit
  , keybindings
  , processInput
  ) where

import Control.Monad
import Foreign.C.Types
import SDL
import qualified SDL

import InputModule
import Entity

-- Hands the current state of the game to various functions
data GameState = 
  State
  { options   :: Options
  , entities  :: [Entity]
  }

-- We will need this later so just making a newtype for now
data Options = 
  Options 
    { screenRes   :: (CInt, CInt)
    , frameLimit  :: Int
    , keybindings :: KeyBindings GameState
    }

initialState :: GameState
initialState = State initialOptions []

initialOptions :: Options
initialOptions =
  Options
    { screenRes = (1440, 1080)
    , frameLimit = 60
    , keybindings = blankKeyBindings :: KeyBindings GameState
    }

-- Change the gamestate with input
processInput :: GameState -> SDL.EventPayload -> GameState
processInput state@(State (Options _ _ kbs) _) input = exec $ join func
  where func = getBoundInput kbs <$> processEvent input
        exec (Just f) = f state
        exec _ = state

-- Retrieves the keycode and press-status of key
processEvent :: SDL.EventPayload -> Maybe (SDL.Keycode, Bool)
processEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed False (SDL.Keysym _ code _))) = Just (code, True)
processEvent (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Released _ (SDL.Keysym _ code _))) = Just (code, False)
processEvent _ = Nothing

-- Updates the game state's entities
updateGameState :: GameState -> CDouble -> GameState
updateGameState state delta = state { entities = map (\(Entity _ up _) -> up delta) (entities state)}

-- Renders the game state's entities
renderGameState :: GameState -> IO [()]
renderGameState state = sequence $ map (\(Entity _ _ rend) -> rend) $ entities state
-- SDL.copy renderer player (getCurrentFrame newAnimState) $ Just $ SDL.Rectangle (truncate <$> position (entities newState)) (V2 100 100)

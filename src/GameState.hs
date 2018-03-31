module GameState  
  ( GameState(State)
  , Entity(Entity)
  , Options(Options)
  , KeyBindings
  , initialState
  , entities
  , options
  , deltaTime
  , elapsedTime
  , updateGameState
  , renderGameState
  , initialOptions
  , screenRes
  , frameLimit
  , keybindings
  , processInput
  , eID
  , update
  , render
  ) where

import Control.Monad
import Foreign.C.Types
import SDL
import qualified SDL

import InputModule

-- Hands the current state of the game to various functions
data GameState = 
  State
  { options     :: Options
  , deltaTime   :: CDouble
  , elapsedTime :: CDouble
  , entities    :: [Entity]
  }

data Entity =
  Entity
  { eID       :: Int
  , update    :: GameState -> Entity
  , render    :: SDL.Renderer -> IO ()
  }

-- We will need this later so just making a newtype for now
data Options = 
  Options 
    { screenRes   :: (CInt, CInt)
    , frameLimit  :: Int
    , keybindings :: KeyBindings GameState
    }

initialState :: GameState
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
processInput state@(State (Options _ _ kbs) _ _ _) input = exec $ join func
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
updateGameState state delta = 
  state 
  { deltaTime = delta
  , elapsedTime = elapsedTime state + delta
  , entities = map (\(Entity _ up _) -> up state) (entities state)
  }

-- Renders the game state's entities
renderGameState :: GameState -> SDL.Renderer -> IO [()]
renderGameState state renderer = sequence $ map (\(Entity _ _ rend) -> rend renderer) $ entities state

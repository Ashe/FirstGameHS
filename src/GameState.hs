module GameState  ( GameState(State)
                  , Guy(Guy)
                  , Options(Options)
                  , KeyBindings
                  , initialState
                  , options
                  , entities
                  , screenRes
                  , frameLimit
                  , position
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

import InputModule

-- Hands the current state of the game to various functions
data GameState = 
  State
  { options   :: Options
  , entities  :: Guy
  }

-- We will need this later so just making a newtype for now
data Options = 
  Options 
    { screenRes   :: (CInt, CInt)
    , frameLimit  :: Int
    , keybindings :: KeyBindings GameState
    }

-- This is our game world. It only consists of one lonely guy
-- who has a position and a velocity
data Guy = 
    Guy
    { position :: Point V2 CDouble
    , velocity :: V2 CDouble
    , tag :: String
    , animation :: String
    , frame :: Int
    } deriving (Show, Eq)

initialState :: GameState
initialState = State initialOptions (initialGuy initialOptions)

-- Our initial guy starts out with him roughly in the middle
initialGuy :: Options -> Guy
initialGuy opts =
    Guy
    { position = P $ V2 (fromIntegral (fst (screenRes opts)) / 2) (fromIntegral $ snd (screenRes opts) - 100)
    , velocity = V2 0 0
    , tag = "male"
    , animation = "idle"
    , frame = 0
    }

initialOptions :: Options
initialOptions =
  Options
    { screenRes = (640, 480)
    , frameLimit = 60
    , keybindings = blankKeyBindings :: KeyBindings GameState
    }

-- Change the gamestate with input
processInput :: GameState -> SDL.EventPayload -> GameState
processInput state@(State (Options _ _ kbs) _) input = exec $ getBoundInput kbs input
  where exec (Just f) = f state
        exec _ = state

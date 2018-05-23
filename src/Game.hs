-- The main loop of the game, binding everything
-- together from options to characters
------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game
( game
, beginGame
) where

import System.Exit (exitSuccess)

import Control.Monad
import Control.Monad.Fix
import SDL.Vect
import SDL (($=))
import qualified SDL
import qualified SDL.Font

import Reflex
import Reflex.SDL2

import Common
import GameSetup
import SDLAnimations
import InputModule

import Guy

-- The main game loop
game :: forall r t m. (ReflexSDL2 r t m, MonadDynamicWriter t [Layer m] m, MonadIO m) => GameSetup -> m () 
game setup = do

  -- When the network is finished setting up
  gameReady <- getPostBuild

  -- Set up time and limit values
  ticks <- getDeltaTickEvent
  limit <- holdDyn (maxFrames $ options setup) never
  unfTime <- foldDyn updateTime (createTime (maxFrames $ options setup)) (attachPromptlyDyn limit ticks)

  -- Filter out non-game ticks
  delta <- holdDyn (createTime 0) (ffilter nextFrame (updated unfTime))

  -- Count when delta fires and compare at different times to calculate fps
  deltaCount <- count $ updated delta

  -- Tick every quarterSecond
  secondCount <- getRecurringTimerEventWithEventCode 0 1000
  deltaStore <- foldDyn (\a (b,_)->(a,b)) (0,0) $ tagPromptlyDyn deltaCount secondCount
  fps <- holdDyn 0 $ uncurry (-) <$> updated deltaStore

  -- Print a message every frame tick
  -- performEvent_ $ fmap (const testPrint) (updated delta)
  
  -- Get the mouse location
  let defMouseM = MouseMotionEventData Nothing (Mouse 0) [] (P $ V2 0 0) (V2 0 0)
  mM <- holdDyn defMouseM =<< getMouseMotionEvent
  mB <- getMouseButtonEvent
  mW <- getMouseWheelEvent

  -- Load a font with respect to Assets folder
  defFont <- getFontFromFile "Assets/Fonts/Hack-Regular.ttf" 20

  -- Create the player
  animsList <- loadAnimations "Assets/rogue.json"
  pTex <- getTextureFromImg (renderer setup) "Assets/rogue.png"
  let animationSet = getAnimationSet "rogue" "male" =<< animsList
      animation = getAnimation "walk" =<< animationSet
      pAnimState = 
        AnimationState animationSet animation [] "idle" 0 0 10

  -- Enter the recursive do block, to allow cyclic dependencies
  rec

    -- Set up a dynamic that fires every tick containing the entire state
    renderTick <- holdDyn initialState $ tagPromptlyDyn state (updated delta)

    -- Every render tick, render the background and all entities
    commitLayer $ ffor renderTick $ \_ -> SDL.copy (renderer setup) (texmex setup) Nothing Nothing
    
    -- Set up the players
    player <- createGuy (updated renderTick) 0 0 (renderer setup) (join (deltaTime <$> state)) pTex pAnimState

    -- commitLayer $ ffor (ffor renderTick $ \(State _ _ ps) -> fmap prepRender ps) (renderGuy (renderer setup))

    -- Show FPS counter
    commitLayer $ ffor fps $ \a -> renderSolidText (renderer setup) defFont (V4 255 255 255 1) (show a) 0 0

    -- Create an initial state using data above
    let initialState =
          State
          { deltaTime = delta
          , mouse = mM
          , ps = [player]
          }

    -- Create the state dynamic
    state <- foldDyn const initialState never

  -- Quit on a quit event
  evQuit <- getQuitEvent
  performEvent_ $ ffor evQuit $ \() -> liftIO $ do
    -- shutdownOn =<< delay 0 evQuit
    SDL.destroyRenderer $ renderer setup
    SDL.destroyWindow $ window setup
    SDL.Font.quit
    SDL.quit
    exitSuccess

-- Start the game loop properly
beginGame :: GameSetup -> IO ()
beginGame gs =
  host () $ do
    (_, dynLayers) <- runDynamicWriterT $ game gs
    performEvent_ $ ffor (updated dynLayers) $ \layers -> do
      rendererDrawColor r $= V4 0 0 0 255
      clear r
      sequence_ layers
      present r
  where w = window gs
        r = renderer gs

-- Function to just print something to the screen
testPrint :: MonadIO m => m ()
testPrint = liftIO $ print "Frame tick"

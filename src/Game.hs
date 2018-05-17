-- The main loop of the game, binding everything
-- together from options to characters
------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

module Game
( game
, beginGame
) where

import Data.Time.Clock
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
game :: (ReflexSDL2 r t m, MonadDynamicWriter t [Layer m] m) => GameSetup -> m () 
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
  -- deltaCountAlt <- delay 1 $ updated deltaCount

  -- Tick every quarterSecond
  secondCount <- getRecurringTimerEventWithEventCode 0 1000
  deltaStore <- foldDyn (\a (b,_)->(a,b)) (0,0) $ tagPromptlyDyn deltaCount secondCount
  fps <- holdDyn 0 $ uncurry (-) <$> updated deltaStore
  -- fps <- holdDyn 0 $ attachPromptlyDynWith (-) deltaCount deltaCountAlt

  -- Print a message every frame tick
  -- performEvent_ $ fmap (const testPrint) (updated delta)

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

    -- Set up the playersHey everyone
    player <- handleGuy state $ createGuy 0 0 (renderer setup) pTex pAnimState
    player1 <- handleGuy state $ createGuy 50 0 (renderer setup) pTex pAnimState
    player2 <- handleGuy state $ createGuy 100 0 (renderer setup) pTex pAnimState
    player3 <- handleGuy state $ createGuy 150 0 (renderer setup) pTex pAnimState
    player4 <- handleGuy state $ createGuy 200 0 (renderer setup) pTex pAnimState
    player5 <- handleGuy state $ createGuy 250 0 (renderer setup) pTex pAnimState
    player6 <- handleGuy state $ createGuy 300 0 (renderer setup) pTex pAnimState

    player7 <- handleGuy state $ createGuy 0 100 (renderer setup) pTex pAnimState
    player8 <- handleGuy state $ createGuy 50 100 (renderer setup) pTex pAnimState
    player9 <- handleGuy state $ createGuy 100 100 (renderer setup) pTex pAnimState
    player10 <- handleGuy state $ createGuy 150 100 (renderer setup) pTex pAnimState
    player11 <- handleGuy state $ createGuy 200 100 (renderer setup) pTex pAnimState
    player12 <- handleGuy state $ createGuy 250 100 (renderer setup) pTex pAnimState
    player13 <- handleGuy state $ createGuy 300 100 (renderer setup) pTex pAnimState

    -- Every tick, render the background and all entities
    commitLayer $ ffor delta $ \_ -> SDL.copy (renderer setup) (texmex setup) Nothing Nothing
    commitLayer $ join $ ffor state $ \(State _ ps) -> renderEntities (\a->Guy.render a a) ps

    -- Show FPS counter
    commitLayer $ ffor fps $ \a -> renderSolidText (renderer setup) defFont (V4 255 255 255 1) (show a) 0 0

    -- Create an initial state using data above
    let initialState =
          State
          { deltaTime = delta
          -- , ps = [player, player1, player2, player3, player4, player5, player6, player7, player8, player9, player10, player11, player12, player13]
          , ps = [player]
          }

    -- Create the state dynamic
    state <- holdDyn initialState never

  -- Quit on a quit event
  evQuit <- getQuitEvent
  performEvent_ $ ffor evQuit $ \() -> liftIO $ do
    SDL.quit
    SDL.destroyRenderer $ renderer setup
    SDL.destroyWindow $ window setup
    SDL.Font.quit
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

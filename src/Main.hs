{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import Control.Monad
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import qualified SDL.Image
import Data.List (foldl')
import SDL.Raw.Timer as SDL
import Text.Pretty.Simple
import Control.Concurrent (threadDelay)
import System.Exit (exitSuccess)

import Reflex
import Reflex.SDL2

import GameState
import SDLAnimations
import InputModule

import Guy

import Paths_FirstGameHS(getDataFileName)

-- Takes file and creates a texture out of it
getTextureFromImg :: SDL.Renderer -> FilePath -> IO SDL.Texture
getTextureFromImg renderer img = do
  surface <- SDL.Image.load =<< getDataFileName img
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  pure texture

--------------------------------------------------------------------------------
-- | An axis aligned bounding box.
data AABB = AABB InputMotion (V2 Int)


--------------------------------------------------------------------------------
-- | Convert a mouse button to an AABB.
mouseButtonToAABB :: MouseButtonEventData -> AABB
mouseButtonToAABB dat = AABB (mouseButtonEventMotion dat) pos
  where P pos32 = mouseButtonEventPos dat
        pos = fromIntegral <$> pos32


--------------------------------------------------------------------------------
-- | Convert a mouse button motion to color.
motionToColor :: InputMotion -> V4 Int
motionToColor Released = V4 255 0 0   128
motionToColor Pressed  = V4 0   0 255 128


--------------------------------------------------------------------------------
-- | Renders an AABB using the handy SDL 2d 'Renderer'.
renderAABB :: MonadIO m => Renderer -> V4 Int -> V2 Int -> m ()
renderAABB r color pos = do
  rendererDrawColor r $= (fromIntegral <$> color)
  fillRect r $ Just $ Rectangle (P $ fromIntegral <$> pos - 10) 20


-------------------------------------------------------------------------------
-- | A type representing one layer in our app.
type Layer m = Performable m ()


----------------------------------------------------------------------
-- | Commit a layer stack that changes over time.
commitLayers :: (ReflexSDL2 r t m, MonadDynamicWriter t [Layer m] m)
      => Dynamic t [Layer m] -> m ()
commitLayers = tellDyn


----------------------------------------------------------------------
-- | Commit one layer that changes over time.
commitLayer :: (ReflexSDL2 r t m, MonadDynamicWriter t [Layer m] m)
            => Dynamic t (Layer m) -> m ()
commitLayer = tellDyn . fmap pure


ffor2 :: Reflex t => Dynamic t a -> Dynamic t b -> (a -> b -> c) -> Dynamic t c
ffor2 a b f = zipDynWith f a b

ffor2up
  :: Reflex t => Dynamic t a -> Dynamic t b1 -> ((a, b1) -> b) -> Dynamic t b
ffor2up a b = ffor (zipDyn a b)


data ButtonState = ButtonStateUp
                 | ButtonStateOver
                 | ButtonStateDown
                 deriving Eq


buttonState :: Bool -> Bool -> ButtonState
buttonState isInside isDown
  | not isInside = ButtonStateUp
  | isDown       = ButtonStateDown
  | otherwise    = ButtonStateOver


button :: (ReflexSDL2 r t m, MonadDynamicWriter t [Layer m] m)
       => Renderer
       -> m (Event t ButtonState)
button r = do
  evMotionData <- getMouseMotionEvent
  let position = V2 100 100
      size     = V2 100 100
      V2 tlx tly = position
      V2 brx bry = position + size
      evMotionPos = fmap fromIntegral . mouseMotionEventPos <$> evMotionData
      evMouseIsInside = ffor evMotionPos $ \(P (V2 x y)) ->
        (x >= tlx && x <= brx) && (y >= tly && y <= bry)
  dMouseIsInside <- holdDyn False evMouseIsInside

  evBtn <- getMouseButtonEvent
  let evBtnIsDown = ffor evBtn $ (== Pressed) . mouseButtonEventMotion
  dButtonIsDown <- holdDyn False evBtnIsDown

  let dButtonStatePre = buttonState <$> dMouseIsInside <*> dButtonIsDown
  evPB         <- getPostBuild
  dButtonState <- holdDyn ButtonStateUp $ leftmost [ updated dButtonStatePre
                                                   , ButtonStateUp <$ evPB
                                                   ]
  commitLayer $ ffor dButtonState $ \st -> do
    let color = case st of
                  ButtonStateUp   -> V4 192 192 192 255
                  ButtonStateOver -> 255
                  ButtonStateDown -> V4 128 128 128 255
    rendererDrawColor r $= color
    fillRect r $ Just $ Rectangle (P position) size

  updated <$> holdUniqDyn dButtonState


guest :: (ReflexSDL2 r t m, MonadDynamicWriter t [Layer m] m) => Window -> Renderer -> m () 
guest window r = do
  -- Print some stuff after the network is built.
  evPB <- getPostBuild
  performEvent_ $ ffor evPB $ \() ->
    liftIO $ putStrLn "starting up..."
  ------------------------------------------------------------------------------
  -- Test async events.
  -- This will wait three seconds before coloring the background black.
  ------------------------------------------------------------------------------
  evDelay <- getAsyncEventWithEventCode 0xBEEF $ threadDelay 3000000
  dDelay  <- holdDyn False $ True <$ evDelay
  commitLayers $ ffor dDelay $ \case
    False -> pure $ do
      rendererDrawColor r $= V4 128 128 128 255
      fillRect r Nothing
    True  -> pure $ do
      rendererDrawColor r $= V4 0 0 0 255
      fillRect r Nothing

  ------------------------------------------------------------------------------
  -- A button!
  ------------------------------------------------------------------------------
  evBtnState <- button r
  let evBtnPressed = fmapMaybe (guard . (== ButtonStateDown)) evBtnState
  performEvent_ $ ffor evBtnPressed $ const $ liftIO $ putStrLn "Button pressed!"

  ------------------------------------------------------------------------------
  -- Ghosty trail of squares
  ------------------------------------------------------------------------------
  -- Gather all mouse motion events into a list, then commit a commitLayers that
  -- renders each move as a quarter alpha'd yello or cyan square.
  evMouseMove <- getMouseMotionEvent
  dMoves      <- foldDyn (\x xs -> take 100 $ x : xs) [] evMouseMove
  commitLayer $ ffor dMoves $ \moves ->
    forM_ (reverse moves) $ \dat -> do
      let P pos = fromIntegral <$> mouseMotionEventPos dat
          color = if null (mouseMotionEventState dat)
                  then V4 255 255 0   128
                  else V4 0   255 255 128
      renderAABB r color pos

  ------------------------------------------------------------------------------
  -- Up and down squares
  ------------------------------------------------------------------------------
  -- Get any mouse button event and accumulate them as a list of
  -- AABBs. Commit a commitLayers of those rendered up/down AABBs.
  evMouseButton <- getMouseButtonEvent
  dBtns         <- foldDyn (\x xs -> take 100 $ x : xs) [] evMouseButton
  commitLayer $ ffor dBtns $ \btns ->
    forM_ (reverse btns) $ \dat -> do
      let AABB motion pos = mouseButtonToAABB dat
          color = motionToColor motion
      renderAABB r color pos

  ------------------------------------------------------------------------------
  -- An ephemeral commitLayers that only renders when a key is down, and only listens
  -- to the tick event while that key is down.
  -- This is an example of the higher-order nature of the reflex network. We
  -- can update the shape of the network in response to events within it.
  ------------------------------------------------------------------------------
  evKey <- getKeyboardEvent
  let evKeyNoRepeat = fmapMaybe (\k -> k <$ guard (not $ keyboardEventRepeat k)) evKey
  dPressed <- holdDyn False $ ((== Pressed) . keyboardEventKeyMotion) <$> evKeyNoRepeat
  void $ holdView (return ()) $ ffor (updated dPressed) $ \case
    False -> return ()
    True  -> do
      evDeltaTick <- getDeltaTickEvent
      dTimePressed <- foldDyn (+) 0 evDeltaTick
      commitLayer $ ffor dTimePressed $ \t -> do
        let wrap :: Float -> Int
            wrap x = if x > 255 then wrap (x - 255) else floor x
            rc    = wrap $ fromIntegral t/1000 * 255
            gc    = wrap $ fromIntegral t/2000 * 255
            bc    = wrap $ fromIntegral t/3000 * 255
            color :: V4 Int
            color = fromIntegral <$> V4 rc gc bc 255
        renderAABB r color 100

  ------------------------------------------------------------------------------
  -- Test our recurring timer events
  ------------------------------------------------------------------------------
  let performDeltaSecondTimer n = do
        evEverySecond  <- getRecurringTimerEventWithEventCode n $ fromIntegral n * 1000
        dSeconds       <- foldDyn (+) (0 :: Int) $ 1 <$ evEverySecond
        evSecondsDelta <- performEventDelta $ updated dSeconds
        dSecondsDelta  <- holdDyn 0 evSecondsDelta
        putDebugLnE (updated $ zipDynWith (,) dSeconds dSecondsDelta) $ (show n ++) . (": " ++) . show
  performDeltaSecondTimer 1
  performDeltaSecondTimer 2

  ------------------------------------------------------------------------------
  -- Quit on a quit event
  ------------------------------------------------------------------------------
  evQuit <- getQuitEvent
  performEvent_ $ ffor evQuit $ \() -> liftIO $ do
    putStrLn "bye!"
    quit
    destroyWindow window
    exitSuccess

main :: IO ()
main = do

  -- Initialise SDL
  SDL.initialize [SDL.InitVideo]

  -- Create a window with the correct screensize and make it appear
  window <- SDL.createWindow "FirstGameHS" SDL.defaultWindow 

  -- Create a renderer for the window for rendering textures
  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedRenderer
        , SDL.rendererTargetTexture = False
        }

-- -- Set up the first state
-- let jump p@(Guy _ curVel _ _) = p { velocity = curVel * V2 1 0 + jumpVelocity }
--     fall p@(Guy _ curVel _ _) = p { velocity = curVel - jumpVelocity }
--     right p@(Guy _ curVel _ _) = p { velocity = walkingSpeed + curVel }
--     left p@(Guy _ curVel _ _) = p { velocity = curVel - walkingSpeed }
--
--     initOptions = initialOptions { keybindings = 
--       addBatchBindings 
--         [ ((SDL.KeycodeUp, True), Just (entManip 0 jump))
--         , ((SDL.KeycodeUp, False), Just (entManip 0 fall))
--         , ((SDL.KeycodeRight, True), Just (entManip 0 right))
--         , ((SDL.KeycodeRight, False), Just (entManip 0 left))
--         , ((SDL.KeycodeLeft, True), Just (entManip 0 left))
--         , ((SDL.KeycodeLeft, False), Just (entManip 0 right))
--         ] blankKeyBindings }
--     state = initialState { options = initOptions }


  -- Load in the background
  background <- getTextureFromImg renderer "Assets/foo.bmp"

  -- Load in the player's texture and animations
  animsList <- loadAnimations "Assets/rogue.json"
  playerTexture <- getTextureFromImg renderer "Assets/rogue.png"
  let animationSet = getAnimationSet "rogue" "male" =<< animsList
      animation = getAnimation "walk" =<< animationSet
      initAnimationState = 
        AnimationState animationSet animation [] "idle" 0 0

      -- Create the player and add it to the entitylist
      player = createGuy 0 0 playerTexture initAnimationState

  -- Create the initial state and put the player in
  let state = (initialState renderer background) { entities = [player]}

  -- Set the window size
  SDL.windowSize window $= uncurry V2 (screenRes (options state))

  -- Show the window
  SDL.showWindow window

  host () $ do
    (_, dynLayers) <- runDynamicWriterT $ guest window renderer
    performEvent_ $ ffor (updated dynLayers) $ \layers -> do
      rendererDrawColor renderer $= V4 0 0 0 255
      clear renderer
      sequence_ layers
      present renderer

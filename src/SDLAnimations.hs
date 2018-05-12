-- This module is responsible for taking animationData 
-- and applying it into an SDL context
------------------------------------------------------

module SDLAnimations 
  ( AnimationSet(..)
  , Animation(..)
  , AnimationState(..)
  , loadAnimations
  , getAnimationSet
  , getAnimation
  , getFrame
  , updateAnimationState
  , getCurrentFrame
  ) where

import Control.Monad.IO.Class
import Foreign.C.Types
import SDL.Vect
import Data.Fixed
import qualified SDL
import qualified SDL.Image

import Common
import AnimationLoader

import Debug.Trace

-- State for keeping track of animations
data AnimationState =
  AnimationState  
    { animationSet      :: Maybe AnimationSet
    , currentAnimation  :: Maybe Animation
    , animationBuffer   :: [String]
    , defaultAnimation  :: String
    , frameNumber       :: Int
    , secondsIntoAnim   :: Double
    , framesPerSecond   :: Int
    } deriving (Show)

-- Non-serializable AnimationSet
data AnimationSet = 
  AnimationSet
    { entityName  :: String
    , tag         :: String
    , animations  :: [Animation]
    } deriving (Show)

-- Non-serializable Animation
data Animation =
  Animation 
    { name    :: String 
    , loop    :: Bool
    , frames  :: [SDL.Rectangle CInt]
    } deriving (Show)

-- Export list of tuples of textures and clips
loadAnimations :: MonadIO m => FilePath -> m (Maybe [AnimationSet])
loadAnimations path = liftIO $ fmap (map convertToAnimationSet) <$> getAnimationDataFromJSON (JSONFile path)

-- Take some data and return a useful datatype
convertToAnimationSet :: AnimationSetData -> AnimationSet
convertToAnimationSet (AnimationSetData entName tag animations) = 
  AnimationSet entName tag $ map convertToAnimation animations
  where convertToAnimation (AnimationData animName loop frames) = 
          Animation animName loop $ map convertToRect frames
        convertToRect (Frame x y w h) = 
          SDL.Rectangle (P $ V2 (fromIntegral x) (fromIntegral y)) (V2 (fromIntegral w) (fromIntegral h))

-- Takes a list of animationsets and returns the correct set
getAnimationSet :: String -> String -> [AnimationSet] -> Maybe AnimationSet
getAnimationSet entity tag set = ret $ filter checkSets set
  where checkSets (AnimationSet entity' tag' _) = entity == entity' && tag == tag'
        ret (x : _) = Just x
        ret [] = Nothing

-- Takes an animation set and returns an animation
getAnimation :: String -> AnimationSet -> Maybe Animation
getAnimation animName (AnimationSet _ _ anims) = ret $ filter checkAnim anims
  where checkAnim (Animation name _ _) = name == animName
        ret (x : xs) = Just x
        ret [] = Nothing

-- Get a frame from an animation quickly
getFrame :: Int -> Animation -> SDL.Rectangle CInt
getFrame frame (Animation _ _ frames) = newNum
  where newNum
          | length frames > frame = frames !! frame
          | otherwise = head frames

-- Add one frame to the animation state, and wrap it round if need be
updateAnimationState :: Time -> AnimationState -> AnimationState
updateAnimationState time state = 
  state
  { secondsIntoAnim = fst nextAnimationTick
  , frameNumber = newFrameNumber $ snd nextAnimationTick }
  where nextAnimationTick = calculateTickCount time state
        newFrameNumber b
          | b = advanceAnimationFrame state
          | otherwise = frameNumber state

-- Increases the tick count and sets a flag if there should be a new frame
calculateTickCount :: Time -> AnimationState -> (Double, Bool)
calculateTickCount time state 
  | ticks >= fps = (mod' ticks fps, nextFrame (currentAnimation state))
  | otherwise = (ticks, False)
  -- | ticks >= fps = Debug.Trace.trace( 
 --  ("\nDeltaTime: " ++ show (delta time)) ++ ("\nElapsed: " ++ show (elapsed time)) ++ ("\nCycles per second: " ++ show fps) ++ ("\nTicks this update: " ++ show ticks)) (mod' ticks fps, nextFrame (currentAnimation state))
 -- | otherwise = Debug.Trace.trace ("\nNO ANIMATION - ticks this update: " ++ show ticks ++ "\n") (ticks, False)
  where fps = (1000 / fromIntegral (framesPerSecond state)) / 1000
        ticks = secondsIntoAnim state + delta time
        nextFrame (Just anim) = loop anim || not (lastFrame anim)
        nextFrame _ = False
        lastFrame anim = frameNumber state >= length (frames anim) - 1

-- Move the animation to the next frame if the tickCount is high
advanceAnimationFrame :: AnimationState -> Int
advanceAnimationFrame state = newFrame
  where nextFrame = frameNumber state + 1
        len (Just x) = length (frames x)
        len _ = 0
        newFrame
          | len (currentAnimation state) > nextFrame = nextFrame
          | otherwise = 0

-- Uses information in the animation state to get the right frame
getCurrentFrame :: AnimationState -> Maybe (SDL.Rectangle CInt)
getCurrentFrame state = getFrame (frameNumber state) <$> currentAnimation state

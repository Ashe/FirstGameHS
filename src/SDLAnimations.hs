-- This module is responsible for taking animationData and applying it into an SDL context

module SDLAnimations ( AnimationSet()
                     , Animation()
                     , AnimationState(AnimationState, currentAnimation, frameNumber, tickCount)
                     , loadAnimations
                     , getAnimationSet
                     , getAnimation
                     , getFrame
                     , updateAnimationState
                     , getCurrentFrame)
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
import qualified SDL.Image

import AnimationLoader

-- State for keeping track of animations
data AnimationState =
  AnimationState  { animationSet      :: Maybe AnimationSet
                  , currentAnimation  :: Maybe Animation
                  , animationBuffer   :: [String]
                  , defaultAnimation  :: String
                  , frameNumber       :: Int
                  , tickCount         :: CDouble
                  } deriving (Show)

-- Non-serializable AnimationSet
data AnimationSet = 
  AnimationSet  { entityName  :: String
                , tag         :: String
                , animations  :: [Animation]
                } deriving (Show)

-- Non-serializable Animation
data Animation =
  Animation { name    :: String 
            , loop    :: Bool
            , frames  :: [SDL.Rectangle CInt]
            } deriving (Show)

-- Export list of tuples of textures and clips
loadAnimations :: FilePath -> IO (Maybe [AnimationSet])
loadAnimations path = fmap (map convertToAnimationSet) <$> getAnimationDataFromJSON (JSONFile path)

-- Take some data and return a useful datatype
convertToAnimationSet :: AnimationSetData -> AnimationSet
convertToAnimationSet (AnimationSetData entName tag animations) = 
  AnimationSet entName tag $ map convertToAnimation animations
  where convertToAnimation (AnimationData animName loop frames) = Animation animName loop $ map convertToRect frames
        convertToRect (Frame x y w h) = SDL.Rectangle (P $ V2 (fromIntegral x) (fromIntegral y)) (V2 (fromIntegral w) (fromIntegral h))

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
updateAnimationState :: CDouble -> CDouble -> AnimationState -> AnimationState
updateAnimationState delta tickRate state = 
  state
  { tickCount = fst nextAnimationTick
  , frameNumber = newFrameNumber $ snd nextAnimationTick }
  where nextAnimationTick = calculateTickCount delta tickRate $ tickCount state
        newFrameNumber b
          | b = advanceAnimationFrame state
          | otherwise = frameNumber state

-- Increases the tick count and sets a flag if there should be a new frame
calculateTickCount :: CDouble -> CDouble -> CDouble -> (CDouble, Bool)
calculateTickCount delta tickRate tickCount 
  | delta + tickCount > tickRate = (delta + tickCount - tickRate, True)
  | otherwise = (delta + tickCount, False)

-- Move the animation to the next frame if the tickCount is high
advanceAnimationFrame :: AnimationState -> Int
advanceAnimationFrame state = newFrame
  where nextFrame = frameNumber state + 1
        len Nothing = 0
        len (Just x) = length (frames x)
        newFrame
          | len (currentAnimation state) > nextFrame = nextFrame
          | otherwise = 0


-- Uses information in the animation state to get the right frame
getCurrentFrame :: AnimationState -> Maybe (SDL.Rectangle CInt)
getCurrentFrame state = getFrame (frameNumber state) <$> currentAnimation state

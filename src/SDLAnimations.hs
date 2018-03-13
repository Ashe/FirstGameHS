-- This module is responsible for taking animationData and applying it into an SDL context

module SDLAnimations ( AnimationSet()
                     , Animation()
                     , loadAnimations
                     , getAnimationSet
                     , getAnimation
                     , getFrame)
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
getFrame frame (Animation _ _ frames) = frames !! frame

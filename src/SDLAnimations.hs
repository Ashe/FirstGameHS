{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- This module is responsible for taking animationData and applying it into an SDL context

module SDLAnimations ( AnimationSet()
                     , Animation()
                     , loadAnimationsFromDir)
  where

import Control.Monad
import System.Directory
import Data.List
import Data.Monoid
import Data.Functor
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
            , frames  :: [SDL.Rectangle Int]
            } deriving (Show)

-- Newtype for image files
newtype IMGFile = IMGFile FilePath

-- Export list of tuples of textures and clips
loadAnimationsFromDir :: SDL.Renderer -> FilePath -> IO [Maybe (SDL.Texture, [AnimationSet])]
loadAnimationsFromDir rend path =
  --filepaths <- getFilteredFileNames path
  --sequence (getAnimation rend <$> filepaths)
  getFilteredFileNames path >>= traverse (getAnimation rend)

-- Returns files where fst = IMG and snd = JSON
getFilteredFileNames :: FilePath -> IO [(IMGFile, JSONFile)]
getFilteredFileNames path = do
  filePath <- listDirectory path
  let sfList ext = sort $ filter (isSuffixOf ext) filePath
  pure $ zip (IMGFile <$> sfList ".bmp") (JSONFile <$> sfList ".json")

-- Takes file and creates a texture out of it
getTextureFromImg :: SDL.Renderer -> IMGFile -> IO SDL.Texture
getTextureFromImg renderer (IMGFile img) = do
  surface <- SDL.Image.load img
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  pure texture

-- Takes a pair of file paths and returns an animation
getAnimation :: SDL.Renderer -> (IMGFile, JSONFile) -> IO (Maybe (SDL.Texture, [AnimationSet]))
getAnimation rend pair = do
  tex <- getTextureFromImg rend $ fst pair
  rects <- getSDLAnimationSetsFromJSON $ snd pair
  pure $ fmap (tex,) rects :: IO (Maybe (SDL.Texture, [AnimationSet]))

-- Takes a file and extracts a list of rectangles
getSDLAnimationSetsFromJSON :: JSONFile -> IO (Maybe [AnimationSet])
getSDLAnimationSetsFromJSON jsonFile =
  fmap (map convertToAnimationSet) <$> getAnimationDataFromJSON jsonFile
    where convertToRect (Frame x y w h) = SDL.Rectangle (P $ V2 x y) (V2 x y)
          convertToAnimation (AnimationData animName loop frames) = Animation animName loop $ map convertToRect frames
          convertToAnimationSet (AnimationSetData entName tag animations) 
            = AnimationSet entName tag $ map convertToAnimation animations

-- First attempt
--  json <- getAnimationSetsFromJSON jsonFile
--  case json of
--    Nothing -> pure Nothing
--    Just list -> pure $ Just $ map convertToAnimationSet list

-- Second attempt
--  getAnimationDataFromJSON :: JSONFile -> IO (Maybe [AnimationSetData])
--  pure . fmap (map convertToAnimationSet) =<< getAnimationDataFromJSON jsonFile

--  Final attempt
--  fmap (map convertToAnimationSet) <$> getAnimationDataFromJSON jsonFile

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

-- This module is responsible for loading in images as animations

module Animations ( loadAnimationsFromDir,
                    spitOutJSON )
  where

import Control.Monad
import Control.Applicative ((<$>))
import Control.Arrow((***))
import GHC.Generics
import Foreign.C.Types
import System.Directory
import Data.List
import Data.Monoid
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy(writeFile)
import Data.Functor
import qualified Data.ByteString.Lazy as B
import SDL
import qualified SDL
import qualified SDL.Image

-- Newtypes for files
newtype IMGFile = IMGFile FilePath
newtype JSONFile = JSONFile FilePath

-- Creation of types for JSON parsing
newtype AnimationSet = AnimationSet [Animation]
  deriving(Show, Generic)
instance ToJSON AnimationSet
instance FromJSON AnimationSet

-- Individual animations
data Animation =
  Animation { name    :: String 
            , frames  :: [Frame]
            , loop    :: Bool
            } deriving (Show, Generic)
instance ToJSON Animation
instance FromJSON Animation

-- Individual frames of an animation
data Frame =
  Frame { rectX :: Int
        , rectY :: Int
        , rectW :: Int
        , rectH :: Int
        } deriving (Show, Generic)
instance ToJSON Frame
instance FromJSON Frame

-- Outputs a sample of frames for animation to the path specified
spitOutJSON :: FilePath -> IO ()
spitOutJSON path = Data.ByteString.Lazy.writeFile path $ 
  encodePretty' config (AnimationSet [Animation "basic_0" [Frame 1 2 3 4, Frame 5 6 7 8] True, Animation "basic_0" [Frame 1 2 3 4, Frame 5 6 7 8] False])
  where config = defConfig { confCompare = keyOrder 
    ["name", "loop",  "rectX", "rectY", "rectW", "rectH"] }

-- Get list of all files
-- Filter for pairs of IMG and JSON files
-- Load the IMG into a texture
-- Load the JSON file and work out list of rectangles for each frame
-- Do this for every file

-- Export list of tuples of textures and clips
loadAnimationsFromDir :: SDL.Renderer -> FilePath -> IO [Maybe (SDL.Texture, [SDL.Rectangle Int])]
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

-- Takes a file and extracts a list of rectangles
getAnimationClips :: JSONFile -> IO (Maybe [SDL.Rectangle Int])
getAnimationClips (JSONFile jsonPath) = do
  let convertToRect (Frame x y w h) = SDL.Rectangle (P $ V2 x y) (V2 x y)
  json <- (eitherDecode <$> B.readFile jsonPath) :: IO (Either String [Frame])
  case json of
    Left err -> putStrLn err $> Nothing
    Right list -> pure $ Just $ map convertToRect list

-- Takes a pair of file paths and returns an animation
getAnimation :: SDL.Renderer -> (IMGFile, JSONFile) -> IO (Maybe (SDL.Texture, [SDL.Rectangle Int]))
getAnimation rend pair = do
  tex <- getTextureFromImg rend $ fst pair
  rects <- getAnimationClips $ snd pair
  pure $ fmap (tex,) rects :: IO (Maybe (SDL.Texture, [SDL.Rectangle Int]))


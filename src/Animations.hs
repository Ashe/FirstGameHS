{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}

-- This module is responsible for loading in images as animations

module Animations ( AnimationSet(),
                    Animation(),
                    loadAnimationsFromDir,
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

-- Creation of types for JSON parsing
data AnimationSetData = 
  AnimationSetData  { entityNameData  :: String
                    , tagData         :: String
                    , animationData   :: [AnimationData]
                    } deriving(Show, Generic)
instance ToJSON AnimationSetData
instance FromJSON AnimationSetData

-- Individual animations
data AnimationData =
  AnimationData { nameData   :: String 
                , loopData   :: Bool
                , frameData  :: [Frame]
                } deriving (Show, Generic)
instance ToJSON AnimationData
instance FromJSON AnimationData

-- Individual frames of an animation
data Frame =
  Frame { rectX :: Int
        , rectY :: Int
        , rectW :: Int
        , rectH :: Int
        } deriving (Show, Generic)
instance ToJSON Frame
instance FromJSON Frame

-- Newtypes for files
newtype IMGFile = IMGFile FilePath
newtype JSONFile = JSONFile FilePath

-- Outputs a sample of frames for animation to the path specified
spitOutJSON :: FilePath -> IO ()
spitOutJSON path = Data.ByteString.Lazy.writeFile path $ 
  encodePretty' config 
    [ AnimationSetData "example_character" "male" [AnimationData "idle_0" True [Frame 1 2 3 4, Frame 5 6 7 8], AnimationData "walk_0" True [Frame 1 2 3 4, Frame 5 6 7 8]]
    , AnimationSetData "example_character" "female" [AnimationData "idle_0" True [Frame 1 2 3 4, Frame 5 6 7 8], AnimationData "walk_0" True [Frame 1 2 3 4, Frame 5 6 7 8]]]
  where config = defConfig { confCompare = keyOrder 
    ["entityName", "tag", "animations", "name", "loop", "tag", "rectX", "rectY", "rectW", "rectH"] }

-- Get list of all files
-- Filter for pairs of IMG and JSON files
-- Load the IMG into a texture
-- Load the JSON file and work out list of rectangles for each frame
-- Do this for every file

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

-- Takes a file and extracts a list of rectangles
getAnimationSets :: JSONFile -> IO (Maybe [AnimationSet])
getAnimationSets (JSONFile jsonPath) = do

  let convertToRect (Frame x y w h) = SDL.Rectangle (P $ V2 x y) (V2 x y)
      convertToAnimation (AnimationData animName loop frames) = Animation animName loop $ map convertToRect frames
      convertToAnimationSet (AnimationSetData entName tag animations) 
        = AnimationSet entName tag $ map convertToAnimation animations

  json <- (eitherDecode <$> B.readFile jsonPath) :: IO (Either String [AnimationSetData])
  case json of
    Left err -> putStrLn err $> Nothing
    Right list -> pure $ Just $ map convertToAnimationSet list

-- Takes a pair of file paths and returns an animation
getAnimation :: SDL.Renderer -> (IMGFile, JSONFile) -> IO (Maybe (SDL.Texture, [AnimationSet]))
getAnimation rend pair = do
  tex <- getTextureFromImg rend $ fst pair
  rects <- getAnimationSets $ snd pair
  pure $ fmap (tex,) rects :: IO (Maybe (SDL.Texture, [AnimationSet]))


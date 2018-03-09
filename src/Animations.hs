{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- This module is responsible for loading in images as animations

module Animations where

import Control.Monad
import Control.Applicative ((<$>))
import GHC.Generics
import Foreign.C.Types
import System.Directory
import Data.List
import Data.Aeson 
import qualified Data.ByteString.Lazy as B
import SDL
import qualified SDL

-- Creation of types for JSON parsing
data Frame =
  Frame { rectX :: Int
        , rectY :: Int
        , rectW :: Int
        , rectH :: Int
        } deriving (Show, Generic)
instance ToJSON Frame
instance FromJSON Frame

-- Get list of all files
-- Filter for pairs of BMP and JSON files
-- Load the BMP into a texture
-- Load the JSON file and work out list of rectangles for each frame
-- Do this for every file
-- Export list of tuples of textures and clips
loadAnimationsFromDir :: FilePath -> [(SDL.Texture, [SDL.Rectangle CInt])]
loadAnimationsFromDir path = undefined

-- Returns files where fst = BMP and snd = JSON
getFilteredFileNames :: FilePath -> IO [(FilePath, FilePath)]
getFilteredFileNames path = do
  filePath <- listDirectory path
  let sfList ext = sort $ filter (isSuffixOf ext) filePath
  return $ zip (sfList ".bmp") (sfList ".json")

-- Takes file and creates a texture out of it
getTextureFromBMP :: SDL.Renderer -> FilePath -> IO SDL.Texture
getTextureFromBMP renderer bmp = do
  surface <- SDL.loadBMP bmp
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  return texture

-- Takes a file and extracts a list of rectangles
-- getAnimationClips :: FilePath -> IO (Either String [SDL.Rectangle Int])
-- getAnimationClips jsonPath = do
--  let convertToRect (Frame x y w h) = SDL.Rectangle (P $ V2 x y) (V2 x y)
--      -- json <- (eitherDecode <$> B.readFile jsonPath) :: IO (Either String [Frame])
--  fmap . fmap (map convertToRect) (eitherDecode <$> B.readFile jsonPath :: IO (Either String [Frame]))

getAnimationClips :: FilePath -> IO (Either String [SDL.Rectangle Int])
getAnimationClips jsonPath = do
  let convertToRect (Frame x y w h) = SDL.Rectangle (P $ V2 x y) (V2 x y)
  json <- (eitherDecode <$> B.readFile jsonPath) :: IO (Either String [Frame])
  case json of
    Left err -> return $ Left (err :: String)
    Right list -> return $ Right $ map convertToRect list

-- Takes a pair of file paths and returns an animation
getAnimation :: (FilePath, FilePath) -> (SDL.Texture, [SDL.Rectangle CInt])
getAnimation pair = undefined

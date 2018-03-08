{-# LANGUAGE OverloadedStrings #-}

-- This module is responsible for loading in images as animations

module Animations where

import Control.Monad
import Foreign.C.Types
import System.Directory
import Data.List
import Data.Aeson
import SDL
import qualified SDL

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
getAnimationClips :: FilePath -> [SDL.Rectangle CInt]
getAnimationClips json = undefined

-- Takes a pair of file paths and returns an animation
getAnimation :: (FilePath, FilePath) -> (SDL.Texture, [SDL.Rectangle CInt])
getAnimation pair = undefined

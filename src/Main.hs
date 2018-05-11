{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL.Vect
import SDL (($=))
import qualified SDL

import qualified SDL.Font

import Game
import GameSetup
import Common(getTextureFromImg)
import SDLAnimations

-- Entry point for the game
main :: IO ()
main = do

  -- Initialise SDL
  SDL.initialize [SDL.InitVideo]
  SDL.Font.initialize

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

  -- Load in the background
  background <- getTextureFromImg renderer "Assets/foo.bmp"

  -- Create the initial state and put the player in
  let setup = initialSetup window renderer background

  -- Set the window size
  SDL.windowSize window $= uncurry V2 (screenRes (options setup))

  -- Show the window
  SDL.showWindow window

  -- Begin the main game loop
  beginGame setup

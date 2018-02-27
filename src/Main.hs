{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL

import Paths_FirstGameHS(getDataFileName)

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

loadBMP :: FilePath -> IO SDL.Surface
loadBMP path = getDataFileName path >>= SDL.loadBMP

main :: IO ()
main = do

  -- Initialise SDL
  SDL.initialize [SDL.InitVideo]

  -- Create a window with the correct screensize and make it appear
  window <- SDL.createWindow "SDL Tutorial"
    SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
  SDL.showWindow window

  -- Create a renderer for the window for rendering textures
  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
        { SDL.rendererType = SDL.AcceleratedRenderer
        , SDL.rendererTargetTexture = False
        }

  SDL.rendererDrawColor renderer $= V4 maxBound maxBound maxBound maxBound

  -- Make a surface from file
  xOutSurface <- getDataFileName "foo.bmp" >>= SDL.loadBMP
  texture <- SDL.createTextureFromSurface renderer xOutSurface

  -- Free the surface as we have a texture now
  SDL.freeSurface xOutSurface

  let loop = do
        events <- SDL.pollEvents

        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        SDL.clear renderer
        SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (P $ V2 0 0) (V2 200 200))
        SDL.present renderer

        unless quit loop

  loop


  SDL.destroyWindow window
  SDL.quit

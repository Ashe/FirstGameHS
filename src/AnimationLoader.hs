{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- This module is responsible for animation data out of JSON for modules that need it
-- It also declares the types necessary to decode it as well as an exemplary function

module AnimationLoader  ( AnimationSetData(AnimationSetData)
                        , AnimationData(AnimationData)
                        , Frame(Frame)
                        , JSONFile(JSONFile)
                        , getAnimationDataFromJSON
                        , spitOutJSON )
  where

import Control.Monad
import Control.Applicative ((<$>))
import GHC.Generics
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy(writeFile)
import Data.Functor
import qualified Data.ByteString.Lazy as B

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

-- Newtype for JSON files
newtype JSONFile = JSONFile FilePath

-- Outputs a sample of frames for animation to the path specified
spitOutJSON :: FilePath -> IO ()
spitOutJSON path = Data.ByteString.Lazy.writeFile path $ 
  encodePretty' config 
    [ AnimationSetData "example_character" "male" 
        [AnimationData "idle_0" True [Frame 1 2 3 4, Frame 5 6 7 8], 
        AnimationData "walk_0" True [Frame 1 2 3 4, Frame 5 6 7 8]]

    , AnimationSetData "example_character" "female" 
        [AnimationData "idle_0" True [Frame 1 2 3 4, Frame 5 6 7 8], 
        AnimationData "walk_0" True [Frame 1 2 3 4, Frame 5 6 7 8]]]

  where config = defConfig { confCompare = keyOrder 
    ["entityName", "tag", "animations", "name", "loop", "tag", "rectX", "rectY", "rectW", "rectH"] }

-- Takes a file and extracts a list of rectangles
getAnimationDataFromJSON :: JSONFile -> IO (Maybe [AnimationSetData])
getAnimationDataFromJSON (JSONFile jsonPath) = do
  json <- (eitherDecode <$> B.readFile jsonPath) :: IO (Either String [AnimationSetData])
  case json of
    Left err -> putStrLn err $> Nothing
    Right list -> pure $ Just list

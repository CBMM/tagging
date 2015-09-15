{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Experiments.HomeAlonePersonAndDirection where

import Control.Error
import qualified Data.Aeson as A
import Data.Char
import Control.Monad (mzero)
import Data.Aeson.Types
import Data.Monoid
import qualified Data.Text as T
import GHC.Generics
import Tagging.Stimulus

clipSet :: StimulusSequence
clipSet =
  StimulusSequence "HomeAloneClips" Nothing "Invididual shots from Home Alone 2"
  "/static/clips/HomeAlone2"

clips :: [StimulusResource]
clips = map (\n -> StimulusResource (T.pack (show n))
                    ("clip" <> T.pack (show n) <> ".mp4")
         "video/mp4") [0..10]


data HomeAloneExperiment

instance Experiment HomeAloneExperiment where
  type Stimulus HomeAloneExperiment = Int
  type Question HomeAloneExperiment = Int
  type Answer   HomeAloneExperiment = [CharacterAtDir]

picsSt = "Simple Pictures"

data HeadInfo = HDLeft | HDLeftMid | HDCenter | HDRightMid | HDRight
              | HDBack | HDOffCamera
  deriving (Eq, Ord, Enum, Show, Read, Generic)

type CharacterName = T.Text

data CharacterAtDir = CharacterAtDir
  { cadName :: CharacterName
  , cadDir  :: Maybe HeadInfo
  } deriving (Eq, Show, Generic)

data CharacterWithFlags = CharacterWithFlags
  { cwfName    :: CharacterName
  , cwfDir     :: HeadInfo
  , cwfTalking :: Bool
  } deriving (Eq, Show, Generic)

instance A.FromJSON HeadInfo where
  parseJSON (String v) = case readMay (T.unpack v) of
    Nothing -> mzero
    Just hd -> return hd
  parseJSON _          = mzero

instance A.ToJSON HeadInfo where
  toJSON d = A.String $ T.pack (show d)

instance A.FromJSON CharacterAtDir where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 3 . map toLower
  }

instance A.ToJSON CharacterAtDir where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 3 . map toLower
  }

instance A.FromJSON CharacterWithFlags where
  parseJSON (Object v) = CharacterWithFlags
                       <$> v .: "name"
                       <*> v .: "headdirection"
                       <*> v .: "talking"

instance A.ToJSON CharacterWithFlags where
  toJSON (CharacterWithFlags n d t) = A.object ["name"          .= n
                                               ,"headdirection" .= d
                                               ,"talking"       .= t
                                               ]

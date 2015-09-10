{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Experiments.HomeAlonePersonAndDirection where

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

data HeadDirection = HDLeft | HDLeftMid | HDCenter | HDRightMid | HDRight
  deriving (Eq, Ord, Enum, Show, Read, Generic)

type CharacterName = T.Text

data CharacterAtDir = CharacterAtDir
  { cadName :: CharacterName
  , cadDir  :: Maybe HeadDirection
  } deriving (Eq, Show, Generic)

instance A.FromJSON HeadDirection where
  parseJSON (Object v) = fmap read (v .: "headdirection")
  parseJSON _          = mzero

instance A.ToJSON HeadDirection where
  toJSON d = A.object ["headdirection" .= show d]

instance A.FromJSON CharacterAtDir where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 3 . map toLower
  }

instance A.ToJSON CharacterAtDir where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 3 . map toLower
  }

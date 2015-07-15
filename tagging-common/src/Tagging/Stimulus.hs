{-# LANGUAGE DeriveGeneric #-}

module Tagging.Stimulus where

import qualified Data.Aeson as A
import GHC.Generics

data StimType = Image | Audio | Video
  deriving (Eq, Show, Ord, Bounded, Enum)

data StimulusSet = Stimulus {
  stimType :: StimType
}

data StimIndex = StimIndex {
    siSetID :: Int
  , siItemID :: Int
} deriving (Eq, Show, Generic)

instance A.FromJSON StimIndex where
instance A.ToJSON   StimIndex where

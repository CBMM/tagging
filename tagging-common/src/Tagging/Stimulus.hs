{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}

module Tagging.Stimulus where

import Data.Aeson
import GHC.Generics

class IsTrial t where

  type Stimulus t
  type Question t
  type Answer   t

  sendTrialData
    :: (ToJSON t, ToJSON (Stimulus t), ToJSON (Question t))
    => t
    -> Stimulus t
    -> Question t
    -> Object



data StimType = Image | Audio | Video
  deriving (Eq, Show, Ord, Bounded, Enum)

data StimulusSet = Stimulus {
  stimType :: StimType
}

data StimIndex = StimIndex {
    siSetID :: Int
  , siItemID :: Int
} deriving (Eq, Show, Generic)

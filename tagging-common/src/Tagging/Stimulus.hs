{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

module Tagging.Stimulus where

import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.TH
import GHC.Generics

-----------------------------------------------------------------------------
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


data StimulusResource = StimResource
  { urlSuffix :: T.Text
  , mimeType  :: T.Text
  } deriving (Generic)

data StimulusSet = StimSet
  { ssTitle       :: T.Text
  , ssDescription :: T.Text
  , ssBaseUrl     :: T.Text
  } deriving (Generic)

data StimulusSequenceItem = StimSeqItem
  { ssiStimSet      :: DefaultKey StimulusSet
  , ssiStimulus     :: DefaultKey StimulusResource
  , ssiIndex        :: Int
  , ssiResponseType :: T.Text
  } deriving (Generic)

instance A.FromJSON StimulusResource where
instance A.ToJSON   StimulusResource where
instance A.FromJSON StimulusSet where
instance A.ToJSON   StimulusSet where
instance A.FromJSON StimulusSequenceItem where
instance A.ToJSON   StimulusSequenceItem where

mkPersist defaultCodegenConfig [groundhog|
definitions:
  - entity: StimulusResource
  - entity: StimulusSet
    keys:
      - name: SsTitle
    constructors:
      - name: StimSet
        uniques:
          - name: SsTitle
            fields: [ssTitle]
  - entity: StimulusSequenceItem
|]

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tagging.Stimulus where

import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.TH
import GHC.Generics

-----------------------------------------------------------------------------
-- | Organizing class for experiments
class Experiment t where

  type Stimulus t :: *
  -- ^ Custom stimulus type for the experiment
  type Question t :: *
  -- ^ Custom question type
  type Answer   t :: *
  -- ^ Type of answers to the question

  experimentResources :: t -> [StimulusResource]

  sendTrialData
    :: (ToJSON t, ToJSON (Stimulus t), ToJSON (Question t))
    => t
    -> Stimulus t
    -> Question t
    -> Object

  getResource :: Stimulus t -> StimulusResource


data StimulusResource = StimResource
  { srName      :: StimName
  , srUrlSuffix :: T.Text
  , srMimeType  :: T.Text
  } deriving (Show, Generic)

type StimName = T.Text

data StimulusSet = StimSet
  { ssName        :: StimSetName
  , ssDescription :: T.Text
  , ssBaseUrl     :: T.Text
  } deriving (Show, Generic)

type StimSetName = T.Text

data StimulusSequenceItem = StimSeqItem
  { ssiStimSet      :: DefaultKey StimulusSet
  , ssiStimulus     :: DefaultKey StimulusResource
  , ssiIndex        :: Int
  , ssiResponseType :: ResponseType
  } deriving (Generic)
deriving instance Show StimulusSequenceItem

type ResponseType = T.Text

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
      - name: SsName
    constructors:
      - name: StimSet
        uniques:
          - name: SsName
            fields: [ssName]
  - entity: StimulusSequenceItem
|]

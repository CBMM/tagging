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
-- | Experiments define a @Stimulus@, @Question@, and @Answer@,
--   as well as methods for deriving these types from untyped
--   @StimulusResource@'s stored in the database
class Experiment t where

  type Stimulus t :: *
  -- ^ Custom stimulus type for the experiment
  type Question t :: *
  -- ^ Custom question type
  type Answer   t :: *
  -- ^ Type of answers to the question

  experimentResources :: t -> [StimulusResource]
  getResource         :: StimulusResource -> IO (Stimulus t)

  sendTrialData
    :: (ToJSON t, ToJSON (Stimulus t), ToJSON (Question t))
    => t
    -> Stimulus t
    -> Question t
    -> Object


data StimulusResource = StimulusResource
  { srName      :: StimulusName
  , srUrlSuffix :: T.Text
  , srMimeType  :: T.Text
  } deriving (Show, Generic)

type StimulusName = T.Text

data StimSeq = StimSeq
  { ssName        :: StimSeqName
  , ssDescription :: T.Text
  , ssBaseUrl     :: T.Text
  } deriving (Show, Generic)

type StimSeqName = T.Text

data StimSeqItem = StimSeqItem
  { ssiStimSeq      :: DefaultKey StimSeq
  , ssiStimulus     :: DefaultKey StimulusResource
  , ssiIndex        :: Int
  , ssiResponseType :: ResponseType
  } deriving (Generic)
deriving instance Show StimSeqItem

type ResponseType = T.Text

instance A.FromJSON StimulusResource where
instance A.ToJSON   StimulusResource where
instance A.FromJSON StimSeq where
instance A.ToJSON   StimSeq where
instance A.FromJSON StimSeqItem where
instance A.ToJSON   StimSeqItem where

mkPersist defaultCodegenConfig [groundhog|
definitions:
  - entity: StimulusResource
  - entity: StimSeq
    keys:
      - name: SsName
    constructors:
      - name: StimSeq
        uniques:
          - name: SsName
            fields: [ssName]
  - entity: StimSeqItem
|]

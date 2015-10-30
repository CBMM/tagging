{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tagging.Stimulus where

import           Data.Aeson
import qualified Data.Aeson       as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString  as BS
import           Data.Char
import qualified Data.Text        as T
import           Data.Time
import           GHC.Generics
import           GHC.Int
import           Servant.Docs

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

data PositionInfo = PositionInfo {
    piStimulusSequence :: (Int64, StimulusSequence)
  , piStimSeqItem      :: (Int64, StimSeqItem )
  } deriving (Eq, Show, Generic)

instance ToJSON PositionInfo where
  toJSON = A.genericToJSON A.defaultOptions { A.fieldLabelModifier =
                                              drop 2 . map toLower }

instance FromJSON PositionInfo where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier =
                                                    drop 2 . map toLower }

type StimulusName = T.Text

data StimulusSequence = StimulusSequence
  { ssName        :: !StimSeqName
  , ssFirstItem   :: Maybe Int64 -- StimSeqItem
  , ssDescription :: !T.Text
  , ssBaseUrl     :: !T.Text
  } deriving (Eq, Show, Generic)

type StimSeqName = T.Text

data StimSeqItem = StimSeqItem
  { ssiStimSeq      :: Int64 -- StimulusSequence Key
  , ssiStimulus     :: Int64 -- StimulusResource Key
  , ssiNextItem     :: Maybe Int64 -- StimSeqItem Key
  , ssiIndex        :: !Int
  , ssiResponseType :: !ResponseType
  } deriving (Eq, Show, Generic)

data StimulusRequest = StimulusRequest
  { sreqUser        :: Int64 -- AuthUser key
  , sreqStimSeqItem :: Int64
  , sreqTime        :: UTCTime
  } deriving (Eq, Show, Generic)
type ResponseType = T.Text

-- TODO defaultToJSON modify the fields to drop 2 chars
instance A.FromJSON StimulusResource where
instance A.ToJSON   StimulusResource where
instance A.FromJSON StimulusSequence where
instance A.ToJSON   StimulusSequence where
instance A.FromJSON StimSeqItem where
instance A.ToJSON   StimSeqItem where
instance A.FromJSON StimulusRequest where
instance A.ToJSON   StimulusRequest where


-----------------------------------------------------------------------------
-- Instances for servant-docs
instance ToSample StimSeqItem where
  toSamples _ = singleSample sampleStimSeqItem


sampleStimSeqItem :: StimSeqItem
sampleStimSeqItem =
  StimSeqItem 1 1 (Just 3) 1 "Preference"

instance ToSample StimulusSequence where
  toSamples _ = singleSample sampleSequence


--instance ToSample (Int64,StimulusSequence) where


sampleSequence :: StimulusSequence
sampleSequence =
  StimulusSequence "SimplePictures" (Just 1)
  "Three pictures of shapes"
  "http://web.mit.edu/greghale/Public/shapes"

instance ToSample StimulusResource where
  toSamples _ = singleSample sampleResource





sampleResource :: StimulusResource
sampleResource = StimulusResource "a" "a.jpg" "image/jpeg"


instance ToSample StimulusRequest where
  toSamples _ = singleSample sampleRequest

--instance ToSample (Int64, StimulusRequest) where
--  toSample _ = Just (1, sampleRequest)


--instance ToSample [(Int64, StimulusRequest)] where
--  toSample _ = Just [(0,sampleRequest)]

sampleRequest :: StimulusRequest
sampleRequest = StimulusRequest 1 1 (UTCTime (fromGregorian 2015 1 1) 0)

instance ToSample PositionInfo where
  toSamples _ = singleSample (PositionInfo (1, sampleSequence)
                                           (2, sampleStimSeqItem)
                                           (3, sampleResource))

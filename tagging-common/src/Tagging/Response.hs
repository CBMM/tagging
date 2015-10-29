{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Tagging.Response where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Time
import Data.Typeable
import qualified Data.Text as T
import GHC.Generics
import GHC.Int
import Servant.Docs

import Tagging.User
import Tagging.Stimulus

-- | A particular response to one stimulus in a sequence
data StimulusResponse = StimulusResponse
  { srUser          :: Int64 -- TaggingUser Key
  -- ^ Tagging User
  , srStim          :: Int64 -- StimSeqItem Key
  -- ^ Stimulus in the sequence
  , srDeliveredTime :: UTCTime
  -- ^ Trial start time (in server's timezone)
  , srRespondedTime :: UTCTime
  -- ^ Trial end time (in server's timezone)
  , srResponseType  :: T.Text
  -- ^ TypeRep text of the response
  , srResponseData  :: A.Value
  -- ^ Response payload (serialized to T.Text)
  } deriving (Eq, Show, Generic)

instance A.FromJSON StimulusResponse where
instance A.ToJSON   StimulusResponse where


newtype ResponsePayload = ResponsePayload {rpBytes :: T.Text}
  deriving (Eq, Show, Generic)

instance A.ToJSON   ResponsePayload where
instance A.FromJSON ResponsePayload where

-----------------------------------------------------------------------------
-- Instances for servant-docs
instance ToSample StimulusResponse where
  toSamples _ = singleSample sampleResponse

sampleResponse :: StimulusResponse
sampleResponse =
  StimulusResponse
  1
  1
  (UTCTime (fromGregorian 2015 08 21) 0)
  (UTCTime (fromGregorian 2015 08 21) 1)
  "SimplePicturePreference (todo fix)"
  "10"

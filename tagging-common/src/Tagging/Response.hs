{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Tagging.Response where

import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import qualified Data.ByteString as BS
import Data.Time
import Data.Typeable
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Database.Groundhog    as G
import qualified Database.Groundhog.TH as G
import GHC.Generics
import GHC.Int
import Servant.Docs

import Tagging.User
import Tagging.Stimulus
import Utils

-- | A particular response to one stimulus in a sequence
data StimulusResponse = StimulusResponse
  { srUser          :: Int64 -- TaggingUser Key
  -- ^ Tagging User
  , srStim          :: PositionInfo
  -- ^ Stimulus in the sequence
  , srSequence      :: G.DefaultKey StimulusSequence
  , srIndex         :: Int64
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


newtype ResponsePayload = ResponsePayload {rpJson :: A.Value}
  deriving (Eq, Show, Generic, A.ToJSON, A.FromJSON)


G.mkPersist ghCodeGen [G.groundhog|
  - entity: StimulusResponse
|]


-----------------------------------------------------------------------------
-- Instances for servant-docs
instance ToSample StimulusResponse where
  toSamples _ = singleSample sampleResponse

-----------------------------------------------------------------------------
-- Instances for servant-docs
instance ToSample ResponsePayload where
  toSamples _ = singleSample (ResponsePayload sampleResponseJson)

sampleResponseJson :: A.Value
sampleResponseJson = A.object ["characters" .=
                               A.Array (V.fromList ["Kevin", "Mom"])]


sampleResponse :: StimulusResponse
sampleResponse =
  StimulusResponse
  1
  (PositionInfo (intToKey 1) 1)
  (intToKey 1)
  1
  (UTCTime (fromGregorian 2015 08 21) 0)
  (UTCTime (fromGregorian 2015 08 21) 1)
  "SimplePicturePreference (todo fix)"
  "10"

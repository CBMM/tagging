{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

module Tagging.Response where

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import Data.Time
import Data.Typeable
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.TH
import GHC.Generics

import Tagging.User
import Tagging.Stimulus

-- | A particular response to one stimulus in a sequence
data StimulusResponse = StimulusResponse
  { srUser          :: DefaultKey TaggingUser
  -- ^ Tagging User
  , srStim          :: DefaultKey StimSeqItem
  -- ^ Stimulus in the sequence
  , srDeliveredTime :: UTCTime
  -- ^ Trial start time (in server's timezone)
  , srRespondedTime :: UTCTime
  -- ^ Trial end time (in server's timezone)
  , srResponseType  :: T.Text
  -- ^ TypeRep text of the response
  , srResponseData  :: T.Text
  -- ^ Response payload (serialized to T.Text)
  } deriving (Generic)

instance A.FromJSON StimulusResponse where
instance A.ToJSON   StimulusResponse where

mkPersist defaultCodegenConfig [groundhog|
  - entity: StimulusResponse
|]

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

data StimulusResponse = StimResponse
  { srUser          :: DefaultKey User
  , srStim          :: DefaultKey StimulusSequenceItem
  , srDeliveredTime :: UTCTime
  , srRespondedTime :: UTCTime
  , srResponseType  :: T.Text
  , srResponseData  :: T.Text
  } deriving (Generic)

instance A.FromJSON StimulusResponse where
instance A.ToJSON   StimulusResponse where

mkPersist defaultCodegenConfig [groundhog|
  - entity: StimulusResponse
|]

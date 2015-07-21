module Tagging.Response where

import qualified Data.ByteString as BS
import Data.Time
import Data.Typeable
import Database.Groundhog

import Tagging.User
import Tagging.Stimulus

data StimulusResponse = StimResponse
  { srUser          :: DefaultKey User
  , srStim          :: DefaultKey StimulusSequenceItem
  , srDeliveredTime :: UTCTime
  , srRespondedTime :: UTCTime
  , srResponseType  :: TypeRep
  , srResponseData  :: BS.ByteString
  }

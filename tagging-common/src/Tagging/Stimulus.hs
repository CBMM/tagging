{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

module Tagging.Stimulus where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics
import Database.Groundhog
import Database.Groundhog.TH
import Database.Groundhog.Postgresql

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
  { urlSuffix :: BS.ByteString
  , mimeType  :: T.Text
  } deriving (Generic)

data StimulusSet = StimSet
  { ssTitle       :: T.Text
  , ssDescription :: T.Text
  , ssBaseUrl     :: BS.ByteString
  } deriving (Generic)

data StimulusSequenceItem = StimSeqItem
  { ssiStimSet      :: DefaultKey StimulusSet
  , ssiStimulus     :: DefaultKey StimulusResource
  , ssiIndex        :: Int
  , ssiResponseType :: TypeRep
  } deriving (Generic)

mkPersist defaultCodegenConfig [groundhog|
definitions:
  - entity: StimulusResource
    autoKey:
      constrName: AutoKey
      default: true
    constructors:
      - name: StimResource
        fields:
          - name: urlSuffix
            dbName: urlSuffix
          - name: mimeType
            dbName: mimeType
  - entity: StimulusSet
|]

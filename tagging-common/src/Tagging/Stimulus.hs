{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tagging.Stimulus where

import           Control.Lens
import           Control.Monad    (mzero)
import           Data.Aeson
import qualified Data.Aeson       as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString  as BS
import           Data.Char
import           Data.Maybe       (fromMaybe)
import qualified Data.Text        as T
import           Data.Time
import qualified Data.Vector      as V
import qualified Database.PostgreSQL.Simple.ToField   as PGS
import qualified Database.PostgreSQL.Simple.FromField as PGS
import qualified Database.Groundhog.Core    as G
import qualified Database.Groundhog.Generic as G
import qualified Database.Groundhog.Postgresql.Array as G
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


data PositionInfo = PositionInfo {
    _piStimulusSequence :: Int64
  , _piStimSeqIndex     :: Int
  } deriving (Eq, Show, Generic)

makeLenses ''PositionInfo

instance ToJSON PositionInfo where
  toJSON = A.genericToJSON A.defaultOptions
             { A.fieldLabelModifier = drop 3 . map toLower }

instance FromJSON PositionInfo where
  parseJSON = A.genericParseJSON A.defaultOptions
                { A.fieldLabelModifier = drop 2 . map toLower }


data StimulusSequence = StimulusSequence
  { ssName        :: !T.Text
  , ssMetaData    :: !A.Value
  , ssItems       :: G.Array StimSeqItem
  , ssDescription :: !T.Text
  , ssBaseUrl     :: !T.Text
  } deriving (Eq, Show, Generic)

type StimSeqName = T.Text

data StimSeqItem = StimSeqItem
  { ssiStimulus     :: A.Value
  , ssiStimulusSequence :: !Int64
  , ssiIndex            :: !Int
  } deriving (Eq, Show, Generic)

instance G.PersistField PositionInfo where
  persistName _ = "PositionInfo"
  toPersistValues = G.primToPersistValue
  fromPersistValues = G.primFromPersistValue
  dbType _ _ = G.DbTypePrimitive G.DbBlob False Nothing Nothing

-- TODO this seems very unsafe!
instance G.PrimitivePersistField PositionInfo where
  toPrimitivePersistValue p a = G.toPrimitivePersistValue p $ A.encode a
  fromPrimitivePersistValue p x = fromMaybe (error "decode error")
                                  $ A.decode
                                  $ G.fromPrimitivePersistValue p x

instance G.NeverNull PositionInfo where


instance G.PersistField StimSeqItem where
  persistName _ = "StimSeqItem"
  toPersistValues = G.primToPersistValue
  fromPersistValues = G.primFromPersistValue
  dbType _ _ = G.DbTypePrimitive G.DbBlob False Nothing Nothing

-- TODO this seems very unsafe!
instance G.PrimitivePersistField StimSeqItem where
  toPrimitivePersistValue p a = G.toPrimitivePersistValue p $ A.encode a
  fromPrimitivePersistValue p x = fromMaybe (error "decode error")
                                  $ A.decode
                                  $ G.fromPrimitivePersistValue p x

data StimulusRequest = StimulusRequest
  { sreqUser        :: Int64         -- AuthUser key
  , sreqStimSeqItem :: PositionInfo
  , sreqTime        :: UTCTime
  } deriving (Eq, Show, Generic)
type ResponseType = T.Text

-- TODO defaultToJSON modify the fields to drop 2 chars
instance A.FromJSON StimulusSequence where
instance A.ToJSON   StimulusSequence where
instance A.FromJSON StimSeqItem where
instance A.ToJSON   StimSeqItem where
instance A.FromJSON StimulusRequest where
instance A.ToJSON   StimulusRequest where

instance PGS.ToField StimSeqItem where
  toField (StimSeqItem v) = PGS.toField v
instance PGS.FromField StimSeqItem where
  fromField a b = StimSeqItem <$> PGS.fromField a b

instance A.ToJSON a => ToJSON (G.Array a) where
  toJSON (G.Array xs) = toJSON xs

instance A.FromJSON a => FromJSON (G.Array a) where
  parseJSON (A.Array xs) = (G.Array . V.toList) <$> traverse parseJSON xs
  parseJSON _            = mzero

instance G.PersistField A.Value where
  persistName _     = "json"
  toPersistValues   = G.primToPersistValue . A.encode
  fromPersistValues = G.primFromPersistValue
  dbType _ _        = G.DbTypePrimitive G.DbString False Nothing Nothing

instance G.PrimitivePersistField A.Value where
  toPrimitivePersistValue p v   = G.toPrimitivePersistValue p $ A.encode v
  fromPrimitivePersistValue p s = fromMaybe A.Null
                                  (A.decode $ G.fromPrimitivePersistValue p s)


-- instance G.PurePersistField A.Value where
--   toPurePersistValues p v = G.toPurePersistValues p $ A.encode v
--   fromPurePersistValues p s = undefined
--                               -- (A.decode $ G.fromPurePersistValues p s)

-----------------------------------------------------------------------------
-- Instances for servant-docs
instance ToSample StimSeqItem where
  toSamples _ = singleSample sampleStimSeqItem


sampleStimSeqItem :: StimSeqItem
sampleStimSeqItem =
  StimSeqItem (A.String "http://example.com/img.png") -- "Preference"

instance ToSample StimulusSequence where
  toSamples _ = singleSample sampleSequence


sampleSequence :: StimulusSequence
sampleSequence =
  StimulusSequence "SimplePictures"
   (A.String "Sample Metadata")
   (G.Array [sampleStimSeqItem])
   "Three pictures of shapes"
   "http://web.mit.edu/greghale/Public/shapes"


instance ToSample StimulusRequest where
  toSamples _ = singleSample sampleRequest

sampleRequest :: StimulusRequest
sampleRequest = StimulusRequest 1 (PositionInfo 1 1)
                (UTCTime (fromGregorian 2015 1 1) 0)

instance ToSample PositionInfo where
  toSamples _ = singleSample (PositionInfo 1 2)

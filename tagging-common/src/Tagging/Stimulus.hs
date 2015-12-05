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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE StandaloneDeriving #-}

module Tagging.Stimulus where

------------------------------------------------------------------------------
import           Control.Lens       hiding ((.=))
import           Control.Monad      (mzero)
import           Data.Aeson
import qualified Data.Aeson         as A
import qualified Data.Aeson.Types   as A
import qualified Data.ByteString    as BS
import           Data.Char
import           Data.Maybe         (fromJust, fromMaybe)
import           Data.Proxy
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as T (toStrict, fromStrict)
import qualified Data.Text.Lazy.Encoding as T
import           Data.Time
import qualified Data.UUID          as U
import qualified Data.Vector        as V
import qualified Database.Groundhog         as G
import qualified Database.Groundhog.Core    as G
import qualified Database.Groundhog.TH      as G
import qualified Database.Groundhog.Generic as G
import qualified Database.Groundhog.TH.Settings
import           GHC.Generics
import           GHC.Int
import           Servant.Docs
import           Utils

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


data StimulusSequence = StimulusSequence
  { ssName        :: !T.Text
  , ssUuid        :: !U.UUID -- An extra key for namespacing the StimSeqItems
  , ssMetaData    :: !A.Value
  , ssDescription :: !T.Text
  , ssBaseUrl     :: !T.Text
  } deriving (Eq, Show, Generic)


G.mkPersist G.defaultCodegenConfig [G.groundhog|
definitions:
  - entity: StimulusSequence
    keys:
      - name: SsName
    constructors:
      - name: StimulusSequence
        uniques:
          - name: SsName
            fields: [ssName]
|]


type StimSeqName = T.Text

data StimSeqItem = StimSeqItem
  { ssiStimulus         :: A.Value
  , ssiStimulusSequence :: G.DefaultKey StimulusSequence
  , ssiIndex            :: !Int
  } deriving (Eq, Show, Generic)


G.mkPersist G.defaultCodegenConfig [G.groundhog|
  - entity: StimSeqItem
    keys:
      - name: SeqAndIndexConstraint
    constructors:
      - name: StimSeqItem
        uniques:
          - name: SeqAndIndexConstraint
            fields: [ssiStimulusSequence, ssiIndex]
|]



data PositionInfo = PositionInfo {
    _piStimulusSequence :: G.DefaultKey StimulusSequence
  , _piStimSeqIndex     :: Int
  } deriving (Generic)

deriving instance Eq PositionInfo
deriving instance Show PositionInfo
--deriving instance Generic PositionInfo

makeLenses ''PositionInfo

instance ToJSON U.UUID where
  toJSON u = A.object ["uuid" .= U.toWords u]

instance FromJSON U.UUID where
  parseJSON (A.Object v) = do
    ws <- v .: "uuid"
    case ws of
      [a,b,c,d] -> return (U.fromWords a b c d)
      _         -> mzero
  parseJSON _ = mzero


instance ToJSON PositionInfo where
  toJSON = A.genericToJSON A.defaultOptions
             { A.fieldLabelModifier = drop 3 . map toLower }

instance FromJSON PositionInfo where
  parseJSON = A.genericParseJSON A.defaultOptions
                { A.fieldLabelModifier = drop 3 . map toLower }



instance G.PersistField PositionInfo where
  persistName _ = "PositionInfo"
  toPersistValues = G.primToPersistValue
  fromPersistValues = G.primFromPersistValue
  dbType _ _ = G.DbTypePrimitive G.DbBlob False Nothing Nothing

-- instance PGS.FromRow StimSeqItem where
--   fromRow = StimSeqItem <$> PGS.field <*> PGS.field <*> PGS.field

-- instance PGS.ToRow StimSeqItem where
--   toRow (StimSeqItem v s i) = [PGS.toField v, PGS.toField s, PGS.toField i]

-- TODO this seems very unsafe!
instance G.PrimitivePersistField PositionInfo where
  toPrimitivePersistValue p a = G.toPrimitivePersistValue p $ A.encode a
  fromPrimitivePersistValue p x = fromMaybe (error "decode error")
                                  $ A.decode
                                  $ G.fromPrimitivePersistValue p x

instance G.NeverNull PositionInfo where


data StimulusRequest = StimulusRequest
  { sreqUser        :: Int64         -- AuthUser key
  , sreqStimSeqItem :: PositionInfo
  , sreqTime        :: UTCTime
  } deriving (Eq, Show, Generic)
type ResponseType = T.Text

G.mkPersist G.defaultCodegenConfig [G.groundhog|
  - entity: StimulusRequest
|]


-- TODO defaultToJSON modify the fields to drop 2 chars
instance A.FromJSON StimulusSequence where
instance A.ToJSON   StimulusSequence where
instance A.FromJSON StimSeqItem where
instance A.ToJSON   StimSeqItem where
instance A.FromJSON StimulusRequest where
instance A.ToJSON   StimulusRequest where

instance G.PersistField A.Value where
  persistName _     = "json"
  toPersistValues   = G.primToPersistValue -- . T.decodeUtf8 . A.encode
  fromPersistValues = G.primFromPersistValue
  dbType _ _        = G.DbTypePrimitive G.DbString False Nothing Nothing

instance G.PrimitivePersistField A.Value where
  toPrimitivePersistValue p v   = G.toPrimitivePersistValue p . T.toStrict . T.decodeUtf8 $ A.encode v
  fromPrimitivePersistValue p s = fromMaybe A.Null
                                  (A.decode . T.encodeUtf8 . T.fromStrict $ G.fromPrimitivePersistValue p s)


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
  StimSeqItem (A.String "http://example.com/img.png") (intToKey 1) 1

instance ToSample StimulusSequence where
  toSamples _ = singleSample sampleSequence


sampleSequence :: StimulusSequence
sampleSequence =
  StimulusSequence "SimplePictures"
   U.nil
   (A.String "Sample Metadata")
   -- (G.Array [sampleStimSeqItem])
   "Three pictures of shapes"
   "http://web.mit.edu/greghale/Public/shapes"


instance ToSample StimulusRequest where
  toSamples _ = singleSample sampleRequest

sampleRequest :: StimulusRequest
sampleRequest = StimulusRequest 1 (PositionInfo (intToKey 1) 1)
                (UTCTime (fromGregorian 2015 1 1) 0)

instance ToSample PositionInfo where
  toSamples _ = singleSample (PositionInfo (intToKey 2) 2)

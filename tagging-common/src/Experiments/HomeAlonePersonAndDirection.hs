{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Experiments.HomeAlonePersonAndDirection where

import Control.Error
import Control.Monad (mzero)
import qualified Data.Aeson as A
import Data.Aeson.Types
import Data.Char
import qualified Data.Text as T
import qualified Data.UUID as U
-- import qualified Database.Groundhog.Postgresql.Array as G
import GHC.Generics
import Tagging.Stimulus

-- TODO: Shouldn't this be in tagging-server Fixtures?
clipSet :: StimulusSequence
clipSet = StimulusSequence "HomeAloneClips"
                           U.nil
                           A.Null
                           "Invididual shots from Home Alone 2"
                           "/static/clips/HomeAlone2"
  where clips :: [StimSeqItem]
        clips = undefined

-- clips :: [StimulusResource]
-- clips = map (\n -> StimulusResource (T.pack (show n))
--                     ("clip" <> T.pack (show n) <> ".mp4")
--          "video/mp4") [0..10]


data HomeAloneExperiment

instance Experiment HomeAloneExperiment where
  type Stimulus HomeAloneExperiment = Int
  type Question HomeAloneExperiment = Int
  type Answer   HomeAloneExperiment = HomeAloneResponse

-- | The responses from the user have this type: Either a per-clip
--   response (one is required per stimulus), or a Sporadic
--   update to relatively stable character properties
data HomeAloneResponse = PerClip [ClipProperties]
                       -- ^ Every clip needs a list of Character data,
                       --   Who is in the clip, are they talking?
                       | Sporadic StableProperties
                       -- ^ Occasional updates to relatively stable properties,
                       --   like gender, good guy / bad guy status, famousness
                       | Survey Value
                       -- ^ A pre-test survey to get some info about the
                       --   subject and their relation to the movie
                     deriving (Eq, Show, Generic)

instance ToJSON   HomeAloneResponse where
instance FromJSON HomeAloneResponse where



data ClipProperties = ClipProperties
  { _cpCharacterName :: CharacterName
  , _cpHeadDir       :: HeadInfo
  , _cpInteracting   :: Bool
  , _cpPain          :: Maybe Bool
  , _cpMentalizing   :: Maybe Bool
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON   ClipProperties where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 3 . map toLower
    , omitNothingFields = True }

instance FromJSON ClipProperties where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 3 . map toLower }

data StableProperties = StableProperties
  { _spCharacterName :: CharacterName
  , _spGender        :: Maybe Gender
  , _spFeeling       :: Maybe GoodBadGuy
  , _spFamous        :: Maybe Bool
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON   StableProperties where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = drop 3 . map toLower }

instance FromJSON StableProperties where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = drop 3 . map toLower }

data HeadInfo = HDLeft | HDFront | HDRight
              | HDBack | HDBody  | HDOffscreen
  deriving (Eq, Ord, Enum, Show, Read, Generic)

data GoodBadGuy = BadGuy | NeutralGuy | GoodGuy
  deriving (Eq, Ord, Enum, Show, Read, Generic)

data Gender = MaleGender | FemaleGender | OtherGender
  deriving (Eq, Ord, Enum, Show, Read, Generic)

type CharacterName = T.Text


instance ToJSON   Gender where
instance FromJSON Gender where
instance ToJSON   GoodBadGuy where
instance FromJSON GoodBadGuy where

-- data CharacterAtDir = CharacterAtDir
--   { cadName :: CharacterName
--   , cadDir  :: Maybe HeadInfo
--   } deriving (Eq, Show, Generic)
--
-- data CharacterWithFlags = CharacterWithFlags
--   { cwfName    :: CharacterName
--   , cwfDir     :: HeadInfo
--   , cwfTalking :: Bool
--   } deriving (Eq, Show, Generic)

instance A.FromJSON HeadInfo where
  parseJSON (String v) = case readMay (T.unpack v) of
    Nothing -> mzero
    Just hd -> return hd
  parseJSON _          = mzero

instance A.ToJSON HeadInfo where
  toJSON d = A.String $ T.pack (show d)

-- instance A.FromJSON CharacterAtDir where
--   parseJSON = genericParseJSON defaultOptions {
--     fieldLabelModifier = drop 3 . map toLower
--   }
--
-- instance A.ToJSON CharacterAtDir where
--   toJSON = genericToJSON defaultOptions {
--     fieldLabelModifier = drop 3 . map toLower
--   }

-- instance A.FromJSON CharacterWithFlags where
--   parseJSON (Object v) = CharacterWithFlags
--                        <$> v .: "name"
--                        <*> v .: "headdirection"
--                        <*> v .: "talking"
--
-- instance A.ToJSON CharacterWithFlags where
--   toJSON (CharacterWithFlags n d t) = A.object ["name"          .= n
--                                                ,"headdirection" .= d
--                                                ,"talking"       .= t
--                                                ]

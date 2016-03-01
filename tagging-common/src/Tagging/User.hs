{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}


module Tagging.User where

import           Control.Error
import           Control.Monad (mzero)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import           Data.Aeson ((.:),(.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base64 as Base64
import           Data.Char (toLower)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import           Data.Time
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import           Database.Groundhog
import           Database.Groundhog.Core
import           Database.Groundhog.Instances
import           Database.Groundhog.TH
import           GHC.Generics
import           GHC.Int

import Servant.Docs

import Tagging.Stimulus
import Utils

instance A.FromJSON TaggingUser where
instance A.ToJSON TaggingUser where

-- | User privileges
data Role =
    Admin
    -- ^ Create and delete any DB entities, assign stims, up/dnload any data
  | Subject
    -- ^ Unprivileged user, may only answer stim questions
  | Researcher
    -- ^ Upload and own stims, sequences. Download data, assign subjects
    --   to stim sets
  deriving (Eq, Show, Read, Generic)

instance A.FromJSON Role where
instance A.ToJSON   Role where


-- | A user in the Tagging system
data TaggingUser = TaggingUser
  { tuId :: Int64
    -- ^ ID matching AuthUser id
  , tuStudentID :: Maybe T.Text
    -- ^ Optional student ID number
  , tuRealName  :: Maybe T.Text
    -- ^ Optional student full name
  , tuRoles     :: [Role]
    -- ^ List of user Roles
  } deriving  (Eq, Show, Generic)


data Assignment = Assignment
  { aUser     :: DefaultKey TaggingUser
  , aSequence :: DefaultKey StimulusSequence
  , aIndex    :: Int
  } deriving (Generic)

deriving instance Eq Assignment

deriving instance Show Assignment
instance A.ToJSON Assignment where
  toJSON (Assignment u k i) = A.object
    [ "user" .= keyToInt u
    , "sequence" .= keyToInt k
    , "index"    .= i
    ]

instance A.FromJSON Assignment where
  parseJSON (A.Object o) = Assignment
    <$> fmap intToKey (o .: "user")
    <*> fmap intToKey (o .: "sequence")
    <*> o .: "index"
  parseJSON _ = mzero


mkPersist ghCodeGen [groundhog|
  - entity: Assignment
    keys:
      - name: auser
    constructors:
      - name: Assignment
        uniques:
          - name: auser
            fields: [aUser]

|]


data APIKey = APIKey
  { kKey     :: UUID.UUID
  , kOwner   :: DefaultKey TaggingUser
  , kExpires :: UTCTime
  }

mkPersist ghCodeGen [groundhog|
  - primitive: Role
    representation: showread
  - entity: APIKey
  - entity: TaggingUser
    keys:
      - name: tuid
    constructors:
      - name: TaggingUser
        uniques:
          - name: tuid
            fields: [tuId]
  |]


instance A.FromJSON APIKey where
  parseJSON (A.Object o) = do
    u <- intToKey <$> o .: "user"
    e <- o .: "expires"
    k <- o .: "key"
    case textToUuid k of
        Just uu -> return (APIKey uu (u :: DefaultKey TaggingUser) e)
        Nothing -> mzero
  parseJSON _ = mzero

textToUuid :: T.Text -> Maybe UUID.UUID
textToUuid t = let bs = T.encodeUtf8 t
                   b6 = hush $ Base64.decode bs :: Maybe BS.ByteString
               in  b6 >>= UUID.fromByteString . BSL.fromStrict

uuidToText :: UUID.UUID -> T.Text
uuidToText u =
  T.decodeUtf8 . Base64.encode . BSL.toStrict $ UUID.toByteString u

------------------------------------------------------------------------------
-- For servant-docs
instance ToSample TaggingUser where
  toSamples _ = singleSample $
    TaggingUser 1
    (Just "922763745")
    (Just "Greg Hale")
    [Admin, Researcher, Subject]

instance ToSample Assignment where
  toSamples _ = singleSample $
    Assignment (intToKey 1) (intToKey 10) 1

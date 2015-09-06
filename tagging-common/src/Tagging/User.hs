{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Tagging.User where

import qualified Data.Aeson as A
import qualified Data.Text as T
import GHC.Generics
import GHC.Int

import Servant.Docs

import Tagging.Stimulus

-- | A user in the Tagging system
data TaggingUser = TaggingUser
  { tuId :: Int64
    -- ^ ID matching AuthUser id
  , tuStudentID :: Maybe T.Text
    -- ^ Optional student ID number
  , tuRealName  :: Maybe T.Text
    -- ^ Optional student full name
  , tuCurrentStimulus :: Maybe Int64 -- StimSeqItem
    -- ^ Current stimulus assignment (`Nothing` for unassigned)
  , tuRoles     :: [Role]
    -- ^ List of user Roles
  } deriving  (Eq, Show, Generic)

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


------------------------------------------------------------------------------
-- For servant-docs
instance ToSample TaggingUser TaggingUser where
  toSample _ = Just sampleTaggingUser

instance ToSample [TaggingUser] [TaggingUser] where
  toSample _ = Just [sampleTaggingUser]

instance ToSample [(Int64,TaggingUser)] [(Int64,TaggingUser)] where
  toSample _ = Just [(1,sampleTaggingUser)]


sampleTaggingUser = TaggingUser 1 (Just "922763745") (Just "Greg Hale")
                    Nothing [Admin, Researcher, Subject]

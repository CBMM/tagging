{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

module Tagging.User where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.TH
import GHC.Generics

import Tagging.Stimulus

-- | A user in the Tagging system
data TaggingUser = TaggingUser
  { tuId :: Int
  -- ^ ID matching AuthUser id
  , tuStudentID :: Maybe Int
  -- ^ Optional student ID number
  , tuRealName  :: Maybe T.Text
  -- ^ Optional student full name
  , tuCurrentStimulus :: Maybe (AutoKey StimulusSequenceItem)
  -- ^ Current stimulus assignment (`Nothing` for unassigned)
  , tuRoles     :: [Role]
  -- ^ List of user Roles
  } deriving  (Generic)

instance A.FromJSON TaggingUser where
instance A.ToJSON TaggingUser where

-- | User privileges
data Role = Admin
          -- ^ Create/delete any DB entities, assign stims, up/dnload any data
          | Subject
          -- ^ Unprivileged user, may only answer stim questions
          | Researcher
          -- ^ Upload and own stims, sequences. Download data, assign subjects
          --   to stim sets
  deriving (Eq, Show, Generic)

mkPersist defaultCodegenConfig [groundhog|
  - entity: TaggingUser
    keys:
      - name: TuId
    constructors:
      - name: TaggingUser
        uniques:
          - name: TuId
            fields: [tuId]
  - entity: Role
    constructors:
      - name: Admin
      - name: Subject
      - name: Researcher
|]


instance A.FromJSON Role where
instance A.ToJSON   Role where

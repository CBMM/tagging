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

data User = TaggingUser
  { tuId :: Int
  , tuStudentID :: Maybe Int
  , tuRealName  :: Maybe T.Text
  , tuCurrentStimulus :: Maybe (AutoKey StimulusSequenceItem)
  , tuRoles     :: [Role]
  } deriving  (Generic)

instance A.FromJSON User where
instance A.ToJSON User where

data Role = Admin | Subject | Researcher
  deriving (Eq, Show, Generic)

mkPersist defaultCodegenConfig [groundhog|
  - entity: User
  - entity: Role
    constructors:
      - name: Admin
      - name: Subject
      - name: Researcher
|]

instance A.FromJSON Role where
instance A.ToJSON   Role where

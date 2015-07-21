{-# LANGUAGE DeriveGeneric #-}

module Tagging.User where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Database.Groundhog
import GHC.Generics


data User = TaggingUser
  { tuId :: Int
  , tuStudentID :: Maybe Int
  , tuRealName  :: Maybe T.Text
  } deriving  (Generic)


instance A.FromJSON User where
instance A.ToJSON User where

data UserRole = UserRole
  { urUser :: DefaultKey User
  , urRole :: Role
  } deriving (Generic)

data Role = Admin | Subject | Researcher
  deriving (Eq, Show, Generic)

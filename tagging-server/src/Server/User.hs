{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module Server.User where

import Control.Error
import Database.Groundhog
import GHC.Int
import Snap.Core
import Snap.Snaplet
import qualified Data.Aeson as A
import Snap.Snaplet.Groundhog.Postgresql

import Tagging.User
import Server.Application
import Server.Utils
import Server.Crud

-- instance Crud TaggingUser where

-- ------------------------------------------------------------------------------
-- getTaggingUser :: AutoKey TaggingUser
--                -> EitherT String (Handler App App) TaggingUser
-- getTaggingUser k = noteT "Bad TaggingUser lookup" . MaybeT $ gh $ get k

-- ------------------------------------------------------------------------------
-- postTaggingUser :: TaggingUser -> Handler App App (AutoKey TaggingUser)
-- postTaggingUser u@TaggingUser{..} = method POST $ do
--   assertRole [Admin, Researcher]
--   gh $ insert u

-- ------------------------------------------------------------------------------
-- putTaggingUser :: AutoKey TaggingUser -> TaggingUser -> Handler App App ()
-- putTaggingUser k u = do
--   assertRole [Admin, Researcher]
--   method PUT $ gh $ replace k u


-- ------------------------------------------------------------------------------
-- deleteUser :: AutoKey TaggingUser -> Handler App App Bool
-- deleteUser k = do
--   u <- gh $ get k
--   gh $ deleteBy k
--   return (isJust u)

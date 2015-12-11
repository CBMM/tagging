{-# LANGUAGE OverloadedStrings #-}

module Server.Experimenter where

import qualified Data.ByteString.Char8 as B8
import Data.Proxy
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Expression
import Database.Groundhog.TH
import GHC.Int
import Control.Error
import Control.Monad
import Snap.Core
import Snap.Snaplet

import Tagging.User
import Tagging.Stimulus

import Server.Database
import Server.Resources
import Server.Utils hiding (intToKey)
import Server.Application
import Utils

------------------------------------------------------------------------------
-- TODO Deprecate
assignUserSeqStart :: Int64 -> Int -> Handler App App ()
assignUserSeqStart userID seqID = do
  assertRole [Admin, Researcher]
  runGH $
    update
    [TuCurrentStimulusField =. Just (PositionInfo (intToKey seqID) 0 )]
    (TuIdField ==. (fromIntegral userID :: Int64))

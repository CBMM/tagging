{-# LANGUAGE OverloadedStrings #-}

module Experimenter where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

import Control.Error
import Control.Monad
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Groundhog.Postgresql
import Tagging.User
import Tagging.Stimulus
import Resources
import Utils
import Application

------------------------------------------------------------------------------
assignUserSeqStart :: Handler App App ()
assignUserSeqStart =
  maybeT
  (finishEarly 400 "Bad params for 'user' and 'seqitem'")
  (const $ return ()) $ do
    uId       <- MaybeT $ getParam "user"
    seqItemId <- MaybeT $ getParam "seqitem"
    --gh $ undefined
    undefined
    --return ()



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
import Server.Utils
import Server.Application

------------------------------------------------------------------------------
assignUserSeqStart :: Handler App App ()
assignUserSeqStart =
  maybeT (finishEarly 400 "Bad params for 'user' and 'seqitem'")
  (\_ -> return ()) $ do

    uId       <- (hoistMaybe . readMay . B8.unpack)
                 =<< (MaybeT $ getParam "user")

    seqItemId <- (hoistMaybe . readMay . B8.unpack) =<<
                 (MaybeT $ getParam "seqitem")

    MaybeT . fmap Just . runGH $
      update
      [TuCurrentStimulusField =. Just (seqItemId :: Int64)]
      (TuIdField ==. (uId :: Int64))




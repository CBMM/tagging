{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Resources where

-----------------------------------------------------------------------------
import Control.Error
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B8
import Data.Monoid
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Core
import GHC.Int
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Groundhog.Postgresql
import Tagging.Stimulus
import Tagging.User
import Tagging.Response
-----------------------------------------------------------------------------
import Server.Utils
import Server.Application
import Server.Crud

instance Crud TaggingUser where
  intToKey _ = TaggingUserKey . PersistInt64

instance Crud StimulusResource where
  intToKey _ = StimulusResourceKey . PersistInt64

instance Crud StimSeq where
  intToKey _ = StimSeqKey . PersistInt64

instance Crud StimSeqItem where
  intToKey _ = StimSeqItemKey . PersistInt64

instance Crud StimulusResponse where
  intToKey _ = StimulusResponseKey . PersistInt64

migrateResources :: Handler App App ()
migrateResources = do
  assertRole [Admin]
  gh $ runMigration $ do
    migrate (undefined :: TaggingUser)
    migrate (undefined :: StimulusResource)
    migrate (undefined :: StimSeq)
    migrate (undefined :: StimSeqItem)
    migrate (undefined :: StimulusResponse)


-----------------------------------------------------------------------------
getUserSequence :: EitherT String (Handler App App) [StimSeqItem]
getUserSequence = do
  TaggingUser{..}  <- noteT "TaggingUser lookup error" getCurrentTaggingUser
  seqElemKey       <- noteT "Usassigned" (hoistMaybe tuCurrentStimulus)
  StimSeqItem{..}  <- noteT "Bad StimSeqItem lookup"
                      . MaybeT . gh $ get seqElemKey
  EitherT $ fmap Right . gh $ select (SsiStimSeqField ==. ssiStimSeq)

-----------------------------------------------------------------------------
getAllUsers :: Handler App App [(AutoKey TaggingUser, TaggingUser)]
getAllUsers = gh $ selectAll

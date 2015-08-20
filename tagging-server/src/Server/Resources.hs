{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}

module Server.Resources where

-----------------------------------------------------------------------------
import Control.Error
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B8
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Core
import GHC.Int
import Servant
import Servant.Server
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Groundhog.Postgresql
import Tagging.Stimulus
import Tagging.User
import Tagging.Response
-----------------------------------------------------------------------------
import API
import Server.Utils
import Server.Application
import Server.Crud

instance Crud TaggingUser where
  intToKey _ = TaggingUserKey . PersistInt64
  intToAuto _ = TaggingUserKey . PersistInt64
  keyToInt (TaggingUserKey (PersistInt64 i)) = i
  autoToInt Proxy (TaggingUserKey (PersistInt64 i)) = i

instance Crud StimulusResource where
  intToKey _ = StimulusResourceKey . PersistInt64
  intToAuto _ = StimulusResourceKey . PersistInt64
  keyToInt (StimulusResourceKey (PersistInt64 i)) = i
  autoToInt Proxy (StimulusResourceKey (PersistInt64 i)) = i


instance Crud StimulusSequence where
  intToKey _ = StimulusSequenceKey . PersistInt64
  intToAuto _ = StimulusSequenceKey . PersistInt64
  keyToInt (StimulusSequenceKey (PersistInt64 i)) = i
  autoToInt Proxy (StimulusSequenceKey (PersistInt64 i)) = i


instance Crud StimSeqItem where
  intToKey _ = StimSeqItemKey . PersistInt64
  intToAuto _ = StimSeqItemKey . PersistInt64
  keyToInt (StimSeqItemKey (PersistInt64 i)) = i
  autoToInt Proxy (StimSeqItemKey (PersistInt64 i)) = i


instance Crud StimulusResponse where
  intToKey _ = StimulusResponseKey . PersistInt64
  intToAuto _ = StimulusResponseKey . PersistInt64
  keyToInt (StimulusResponseKey (PersistInt64 i)) = i
  autoToInt Proxy (StimulusResponseKey (PersistInt64 i)) = i


migrateResources :: Handler App App ()
migrateResources = do
  assertRole [Admin]
  gh $ runMigration $ do
    migrate (undefined :: TaggingUser)
    migrate (undefined :: StimulusResource)
    migrate (undefined :: StimulusSequence)
    migrate (undefined :: StimSeqItem)
    migrate (undefined :: StimulusResponse)

resourceServer :: Server ResourcesAPI AppHandler
resourceServer = crudServer Proxy :<|> crudServer Proxy :<|> crudServer Proxy
            :<|> crudServer Proxy :<|> crudServer Proxy

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

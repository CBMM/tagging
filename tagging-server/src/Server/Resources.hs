{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Server.Resources where

-----------------------------------------------------------------------------
import Control.Error
import Control.Monad.IO.Class (MonadIO, liftIO)
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
import Tagging.API
import Tagging.Stimulus
import Tagging.User
import Tagging.Response
-----------------------------------------------------------------------------
import Server.Utils
import Server.Application
import Server.Utils
import Server.Crud
import Server.Database
import Utils


resourceServer :: Server ResourcesAPI AppHandler
resourceServer = crudServer Proxy :<|> crudServer Proxy :<|> crudServer Proxy
            :<|> crudServer Proxy :<|> crudServer Proxy :<|> crudServer Proxy

instance HasKey TaggingUser where
  intToKey _ = TaggingUserKey . PersistInt64
  intToAuto _ = TaggingUserKey . PersistInt64
  keyToInt (TaggingUserKey (PersistInt64 i)) = i
  autoToInt Proxy (TaggingUserKey (PersistInt64 i)) = i

instance Crud TaggingUser where

instance HasKey Assignment where
  intToKey _ = AssignmentKey . PersistInt64
  intToAuto _ = AssignmentKey . PersistInt64
  keyToInt (AssignmentKey (PersistInt64 i)) = i
  autoToInt Proxy (AssignmentKey (PersistInt64 i)) = i

instance Crud Assignment where



instance HasKey StimulusSequence where
  intToKey _ = StimulusSequenceKey . PersistInt64
  intToAuto _ = StimulusSequenceKey . PersistInt64
  keyToInt (StimulusSequenceKey (PersistInt64 i)) = i
  autoToInt Proxy (StimulusSequenceKey (PersistInt64 i)) = i

instance Crud StimulusSequence

instance HasKey StimulusResponse where
  intToKey _ = StimulusResponseKey . PersistInt64
  intToAuto _ = StimulusResponseKey . PersistInt64
  keyToInt (StimulusResponseKey (PersistInt64 i)) = i
  autoToInt Proxy (StimulusResponseKey (PersistInt64 i)) = i

instance Crud StimSeqItem

instance HasKey StimSeqItem where
  intToKey _ = StimSeqItemKey . PersistInt64
  intToAuto _ = StimSeqItemKey . PersistInt64
  keyToInt (StimSeqItemKey (PersistInt64 i)) = i
  autoToInt Proxy (StimSeqItemKey (PersistInt64 i)) = i


instance Crud StimulusResponse

instance HasKey StimulusRequest where
  intToKey _ = StimulusRequestKey . PersistInt64
  intToAuto _ = StimulusRequestKey . PersistInt64
  keyToInt (StimulusRequestKey (PersistInt64 i)) = i
  autoToInt Proxy (StimulusRequestKey (PersistInt64 i)) = i

instance Crud StimulusRequest

migrateResourcesIO :: (MonadIO m, PersistBackend m) => m ()
migrateResourcesIO = runMigration $ do
  migrate (undefined :: TaggingUser)
  migrate (undefined :: StimulusSequence)
  migrate (undefined :: StimSeqItem)
  migrate (undefined :: StimSeqAnswer)
  migrate (undefined :: StimulusRequest)
  migrate (undefined :: StimulusResponse)
  migrate (undefined :: SequenceMetadata)
  migrate (undefined :: APIKey)
  migrate (undefined :: Assignment)

 -- selectAll >>= mapM_ (\(ku,u) -> case tuCurrentStimulus u of
 --                         Nothing                 -> return ()
 --                         Just (PositionInfo k i) -> do
 --                           -- Add an assignment
 --                           insert (Assignment ku k i)
 --                           -- Drop (deprecated) posInfo
 --                           replace ku (u {tuCurrentStimulus = Nothing})
 --                      )
 -- --selectAll >>= mapM_ (\(kReq,req :: StimulusRequest) -> liftIO (print req))

 -- selectAll >>= mapM_ (\(kReq,req) ->
 --                        let (PositionInfo k i) = sreqStimSeqItem req
 --                        in  replace kReq (req { sreqSequence = (Utils.intToKey 4)
 --                                              , sreqIndex = fromIntegral i}))
 -- selectAll >>= mapM_ (\(kResp,resp) ->
 --                        let (PositionInfo k i) = srStim resp
 --                        in  replace kResp (resp { srSequence = (Utils.intToKey 4)
 --                                                , srIndex = fromIntegral i}))




migrateHandler :: Handler App App ()
migrateHandler = do
  assertRole [Admin]
  runGH migrateResourcesIO


-----------------------------------------------------------------------------
getAllUsers :: Handler App App [(AutoKey TaggingUser, TaggingUser)]
getAllUsers = runGH selectAll

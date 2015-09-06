{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Server.Resources where

-----------------------------------------------------------------------------
import Control.Error
import           Control.Monad.IO.Class (MonadIO, liftIO)
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
import Server.Utils
import Server.Application
import Server.Crud
import Server.Database


------------------------------------------------------------------------------
type ResourcesAPI =
       "tagginguser"      :> CrudAPI TaggingUser
  :<|> "stimulusresource" :> CrudAPI StimulusResource
  :<|> "stimulussequence" :> CrudAPI StimulusSequence
  :<|> "stimseqitem"      :> CrudAPI StimSeqItem
  :<|> "stimulusresponse" :> CrudAPI StimulusResponse
  :<|> "stimulusrequest"  :> CrudAPI StimulusRequest

resourceServer :: Server ResourcesAPI AppHandler
resourceServer = crudServer Proxy :<|> crudServer Proxy :<|> crudServer Proxy
            :<|> crudServer Proxy :<|> crudServer Proxy :<|> crudServer Proxy

instance HasKey TaggingUser where
  intToKey _ = TaggingUserKey . PersistInt64
  intToAuto _ = TaggingUserKey . PersistInt64
  keyToInt (TaggingUserKey (PersistInt64 i)) = i
  autoToInt Proxy (TaggingUserKey (PersistInt64 i)) = i

instance Crud TaggingUser where

instance HasKey StimulusResource where
  intToKey _ = StimulusResourceKey . PersistInt64
  intToAuto _ = StimulusResourceKey . PersistInt64
  keyToInt (StimulusResourceKey (PersistInt64 i)) = i
  autoToInt Proxy (StimulusResourceKey (PersistInt64 i)) = i

instance Crud StimulusResource

instance HasKey StimulusSequence where
  intToKey _ = StimulusSequenceKey . PersistInt64
  intToAuto _ = StimulusSequenceKey . PersistInt64
  keyToInt (StimulusSequenceKey (PersistInt64 i)) = i
  autoToInt Proxy (StimulusSequenceKey (PersistInt64 i)) = i

instance Crud StimulusSequence

instance HasKey StimSeqItem where
  intToKey _ = StimSeqItemKey . PersistInt64
  intToAuto _ = StimSeqItemKey . PersistInt64
  keyToInt (StimSeqItemKey (PersistInt64 i)) = i
  autoToInt Proxy (StimSeqItemKey (PersistInt64 i)) = i

instance Crud StimSeqItem

instance HasKey StimulusResponse where
  intToKey _ = StimulusResponseKey . PersistInt64
  intToAuto _ = StimulusResponseKey . PersistInt64
  keyToInt (StimulusResponseKey (PersistInt64 i)) = i
  autoToInt Proxy (StimulusResponseKey (PersistInt64 i)) = i

instance Crud StimulusResponse

instance HasKey StimulusRequest where
  intToKey _ = StimulusRequestKey . PersistInt64
  intToAuto _ = StimulusRequestKey . PersistInt64
  keyToInt (StimulusRequestKey (PersistInt64 i)) = i
  autoToInt Proxy (StimulusRequestKey (PersistInt64 i)) = i

instance Crud StimulusRequest

migrateResources :: Handler App App ()
migrateResources = do
  assertRole [Admin]
  gh $ runMigration $ do
    migrate (undefined :: TaggingUser)
    migrate (undefined :: StimulusResource)
    migrate (undefined :: StimulusSequence)
    migrate (undefined :: StimSeqItem)
    migrate (undefined :: StimulusResponse)


-----------------------------------------------------------------------------
getUserSequence :: EitherT String (Handler App App) [StimSeqItem]
getUserSequence = do
  TaggingUser{..}  <- getCurrentTaggingUser
  seqElemKey       <- noteT "Usassigned" (hoistMaybe tuCurrentStimulus)
  StimSeqItem{..}  <- noteT "Bad StimSeqItem lookup"
                      . MaybeT . gh $ get (intToKey Proxy seqElemKey)
  EitherT $ fmap Right . gh $ select (SsiStimSeqField ==. ssiStimSeq)

-----------------------------------------------------------------------------
getAllUsers :: Handler App App [(AutoKey TaggingUser, TaggingUser)]
getAllUsers = gh $ selectAll

------------------------------------------------------------------------------
addStimulusSequence
  :: (MonadIO b, PersistBackend b)
  => Key StimulusSequence BackendSpecific
  -> StimulusSequence
  -> [StimSeqItem]
  -> b (AutoKey StimulusSequence)
addStimulusSequence seqKey seq [] = insert seq
addStimulusSequence seqKey seq (x:xs) = do
  liftIO $ print "About to insert first item:"
  liftIO $ print x
  itemInt0 <- insert (x   :: StimSeqItem)
  let itemKey0 = keyToInt itemInt0
  liftIO $ print "About to replace"
  replace seqKey (seq {ssFirstItem = Just itemKey0} :: StimulusSequence)
  liftIO $ print "About to enter Go loop"
  go itemInt0 xs
  liftIO $ print "About to return"
  return seqKey
  where go parentKey []     = do
          --liftIO $ print "go loop terminal case"
          return ()
        go parentKey (x:xs) = do
          liftIO $ putStrLn ("Inserting " ++ show x)
          k  <- insert x
          v0 <- get parentKey
          maybe (error "Database insertion error")
            (\v0' -> replace parentKey (v0' {ssiNextItem = Just (keyToInt k)}))
            v0
          go k xs

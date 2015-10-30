{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies     #-}

module Server.Utils where

import Control.Error
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (intersect)
import Data.Proxy
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Core
import GHC.Int

import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth hiding (Role)

import Tagging.User
import Tagging.Stimulus
import Tagging.Response
import Server.Application
import Server.Database

class (PersistEntity v,
      PrimitivePersistField (Key v BackendSpecific),
      A.ToJSON (Key v BackendSpecific),
      A.ToJSON (AutoKey v),
      Read (Key v BackendSpecific)) => HasKey v where

  intToKey :: Proxy v -> Int64 -> Key v BackendSpecific
  keyToInt :: Key v BackendSpecific -> Int64
  intToAuto :: Proxy v -> Int64 -> AutoKey v
  autoToInt :: Proxy v -> AutoKey v -> Int64

------------------------------------------------------------------------------
getCurrentTaggingUser :: EitherT String (Handler App App) TaggingUser
getCurrentTaggingUser = do
  lift $ modifyResponse $ setHeader "Cache-Control" "no-cache"
  cu <- noteT "No authUser" $ MaybeT $ with auth currentUser
  case readMay . T.unpack . unUid =<< userId cu of
    Nothing         -> EitherT (return $ Left "Could not read userId")
    Just (i :: Int) ->
      noteT "Zero matches in lookup" $ MaybeT $ fmap listToMaybe $ runGH $
      select (TuIdField ==. (fromIntegral i :: Int64))


------------------------------------------------------------------------------
assertRole :: [Role] -> Handler App App ()
assertRole okRoles = eitherT Server.Utils.err300 return $ do
  TaggingUser{..} <- getCurrentTaggingUser
  when (null (okRoles `intersect` tuRoles)) (lift forbidden)
  return ()


------------------------------------------------------------------------------
forbidden :: MonadSnap m => m ()
forbidden = finishEarly 403 "Can't access that"

------------------------------------------------------------------------------
-- | Utilities copied from Snap.Extras
finishEarly :: MonadSnap m => Int -> BS.ByteString -> m b
finishEarly code str = do
  modifyResponse $ setResponseStatus code str
  modifyResponse $ setHeader "Content-Type" "text/plain"
  writeBS str
  getResponse >>= finishWith


------------------------------------------------------------------------------
serverError :: MonadSnap m => BS.ByteString -> m b
serverError =  finishEarly 500


------------------------------------------------------------------------------
notFound :: MonadSnap m => BS.ByteString -> m b
notFound = finishEarly 404


------------------------------------------------------------------------------
json :: (MonadSnap m, A.ToJSON a) => a -> m ()
json a = do
  modifyResponse $ addHeader "Content-Type" "application/json"
  writeBS (BSL.toStrict . A.encode $ a)

------------------------------------------------------------------------------
err300 :: String -> Handler App App b
err300 = finishEarly 300 . BS.pack

-- instance HasKey TaggingUser where
--   intToKey _ i = TaggingUserKey (PersistInt64 i)
-- instance HasKey StimulusResource where
--   intToKey _ i = StimulusResourceKey (PersistInt64 i)
-- instance HasKey StimulusSequence where
--   intToKey _ i = StimulusSequenceKey (PersistInt64 i)
-- instance HasKey StimulusResponse where
--   intToKey _ i = StimulusResponseKey (PersistInt64 i)
-- instance HasKey StimSeqItem where
--   intToKey _ i = StimSeqItemKey (PersistInt64 i)

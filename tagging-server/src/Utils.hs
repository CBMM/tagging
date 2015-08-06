{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module Utils where

import Control.Error
import Control.Monad
import Control.Monad.Trans.Class
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Auth hiding (Role)
import Snap.Snaplet.Groundhog.Postgresql
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (intersect)
import qualified Data.Text as T
import Database.Groundhog
import GHC.Int

import Tagging.User
import Application


getTaggingUserById :: AutoKey TaggingUser
                   -> EitherT String (Handler App App) TaggingUser
getTaggingUserById tId =
  EitherT $ fmap (note "Bad TaggingUser lookup") $ gh $ get tId

getCurrentTaggingUser :: MaybeT (Handler App App) TaggingUser
getCurrentTaggingUser = do
  cu <- MaybeT $ with auth currentUser
  case readMay . T.unpack . unUid =<< userId cu of
    Nothing         -> MaybeT $ return Nothing
    Just (i :: Int) ->
      MaybeT $ fmap listToMaybe $ gh $ select (TuIdField ==. i)

assertRole :: [Role] -> Handler App App ()
assertRole okRoles = void . runMaybeT $ do
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

serverError :: MonadSnap m => BS.ByteString -> m b
serverError =  finishEarly 500

notFound :: MonadSnap m => BS.ByteString -> m b
notFound = finishEarly 404

json :: (MonadSnap m, A.ToJSON a) => a -> m ()
json a = do
  modifyResponse $ addHeader "Content-Type" "application/json"
  writeBS (BSL.toStrict . A.encode $ a)

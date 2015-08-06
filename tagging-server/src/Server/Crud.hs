{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Crud where

import Control.Error
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B8
import Data.Proxy
import Data.Typeable
import Database.Groundhog
import Database.Groundhog.Core
import GHC.Int
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Groundhog.Postgresql

import Server.Application
import Server.Utils
import Tagging.User

------------------------------------------------------------------------------
class (A.ToJSON v,
       A.FromJSON v,
       PersistEntity v,
       PrimitivePersistField (Key v BackendSpecific),
       Read (Key v BackendSpecific)
      ) => Crud v where

  crudGet :: Key v BackendSpecific -> EitherT String (Handler App App) v
  crudGet = getEntity

  handleGet :: Proxy v -> Handler App App ()
  handleGet _ = do
    eitherT (undefined) (const $ return ()) $ do
      (getId :: Key v BackendSpecific) <-
         (hoistEither . note "Bad id parse" . readMay . B8.unpack)
         =<< (EitherT . fmap (note "No id param") $ getParam "id")
      crudGet getId

  crudPost :: v -> Handler App App (AutoKey v)
  crudPost = postEntity

  handlePost :: Proxy v -> Handler App App ()
  handlePost _ = eitherT undefined (const $ return ()) $ do
    (v :: v) <- EitherT $ A.eitherDecode <$> readRequestBody 100000
    EitherT . fmap Right $ postEntity v

  crudPut :: Key v BackendSpecific -> v -> Handler App App ()
  crudPut = putEntity

  handlePut :: Proxy v -> Handler App App ()
  handlePut _ = eitherT (undefined) (const $ return ()) $ do
    (putId :: Key k BackendSpecific) <-
      (hoistEither . note "Bad id parse" . readMay . B8.unpack)
      =<< (EitherT . fmap (note "No id param") $ getParam "id")
    (v :: v) <- EitherT $ A.eitherDecode <$> readRequestBody 1000000
    EitherT . fmap Right $ putEntity putId v

  crudDelete :: Key v BackendSpecific -> Handler App App Bool
  crudDelete = deleteEntity

  handleDelete :: Proxy v -> Handler App App ()
  handleDelete _ = eitherT (undefined) (const $ return ()) $ do
    (delId :: Key v BackendSpecific) <-
       (hoistEither . note "Bad id parse" . readMay . B8.unpack)
       =<< (EitherT . fmap (note "No id param") $ getParam "id")
    EitherT . fmap Right $ crudDelete delId

  --routes :: Proxy v -> [(B8.ByteString, Handler App App ())]
  --routes p = let tName = show (typeRep $ undefined `asProxyTypeOf` p)
  --           in  zipWith (,) ["get","post","put","delete"] [crudGet, crudPost, crudPut, crudDelete]

------------------------------------------------------------------------------
getEntity :: (PersistEntity a, PrimitivePersistField (Key a BackendSpecific))
             => Key a BackendSpecific
             -> EitherT String (Handler App App) a
getEntity k = noteT "Bad entity lookup" . MaybeT $ gh $ get k


------------------------------------------------------------------------------
postEntity :: (PersistEntity a) => a -> Handler App App (AutoKey a)
postEntity u = method POST $ do
  assertRole [Admin, Researcher]
  gh $ insert u


------------------------------------------------------------------------------
putEntity :: (PersistEntity a, PrimitivePersistField (Key a BackendSpecific))
          => Key a BackendSpecific
          -> a
          -> Handler App App ()
putEntity k u = do
  assertRole [Admin, Researcher]
  method PUT $ gh $ replace k u


------------------------------------------------------------------------------
deleteEntity
  :: (PersistEntity a, PrimitivePersistField (Key a BackendSpecific))
  => Key a BackendSpecific
  -> Handler App App Bool
deleteEntity k = do
  u <- gh $ get k
  gh $ deleteBy k
  return (isJust u)

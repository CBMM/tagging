{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Crud where

import Control.Error
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B8
import Data.Monoid
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

-------------------------------------------------------------------------------
class (A.ToJSON v,
       A.FromJSON v,
       PersistEntity v,
       PrimitivePersistField (Key v BackendSpecific),
       A.ToJSON (AutoKey v),
       Read (Key v BackendSpecific)
      ) => Crud v where

  ------------------------------------------------------------------------
  crudGet :: Key v BackendSpecific -> EitherT String (Handler App App) v
  crudGet = getEntity

  ------------------------------------------------------------------------
  handleGet :: Proxy v -> Handler App App ()
  handleGet _ = do
    getId <- getParam "id"
    case getId of
      Nothing ->
        json =<< (gh selectAll :: Handler App App [(AutoKey v, v)])
      Just s  ->
        eitherT err300 json $ do
          k <- hoistEither . note "Bad id parse" . readMay $ B8.unpack s
          crudGet (k :: Key v BackendSpecific)

  ------------------------------------------------------------------------
  crudPost :: v -> Handler App App (AutoKey v)
  crudPost = postEntity

  ------------------------------------------------------------------------
  handlePost :: Proxy v -> Handler App App ()
  handlePost _ = eitherT err300 json $ do
    (v :: v) <- EitherT $ A.eitherDecode <$> readRequestBody 100000
    (k :: AutoKey v) <- EitherT . fmap Right $ postEntity v
    return (k)

  ------------------------------------------------------------------------
  crudPut :: Key v BackendSpecific -> v -> Handler App App ()
  crudPut = putEntity

  ------------------------------------------------------------------------
  handlePut :: Proxy v -> Handler App App ()
  handlePut _ =
    eitherT err300 (const $ return ()) $ do
      (putId :: Key k BackendSpecific) <-
        (hoistEither . note "Bad id parse" . readMay . B8.unpack)
        =<< (EitherT . fmap (note "No id param") $ getParam "id")
      (v :: v) <- EitherT $ A.eitherDecode <$> readRequestBody 1000000
      EitherT . fmap Right $ putEntity putId v

  crudDelete :: Key v BackendSpecific -> Handler App App Bool
  crudDelete = deleteEntity

  ------------------------------------------------------------------------
  handleDelete :: Proxy v -> Handler App App ()
  handleDelete _ = eitherT err300 (const $ return ()) $ do
    (delId :: Key v BackendSpecific) <-
       (hoistEither . note "Bad id parse" . readMay . B8.unpack)
       =<< (EitherT . fmap (note "No id param") $ getParam "id")
    EitherT . fmap Right $ crudDelete delId

  ------------------------------------------------------------------------
  crudRoutes :: Typeable v
             => Proxy v
             -> [(B8.ByteString, Handler App App ())]
  crudRoutes p =
    let tName = B8.pack $ show (typeRep p)
    in  [(tName,        method GET    (handleGet p))
        ,(tName <> "s", method GET    (handleGet p))
        ,(tName,        method POST   (handlePost p))
        ,(tName,        method PUT    (handlePut p))
        ,(tName,        method DELETE (handleDelete p))
        ]

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

------------------------------------------------------------------------------
err300 :: String -> Handler App App b
err300 = finishEarly 300 . B8.pack

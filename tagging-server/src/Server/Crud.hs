{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds           #-}

module Server.Crud where

import Control.Arrow (first)
import Control.Error
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
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
import Servant        hiding (err300, GET, POST, PUT, DELETE)
import Servant.Server hiding (err300)

import Server.Application
import Server.Database
import Server.Utils
import Tagging.User

-------------------------------------------------------------------------------
class (A.ToJSON v,
       A.FromJSON v,
       HasKey v
       --PersistEntity v,
       --PrimitivePersistField (Key v BackendSpecific),
       --A.ToJSON (AutoKey v),
       --A.ToJSON (Key v BackendSpecific),
       --Read (Key v BackendSpecific)
      ) => Crud v where

  -- intToKey :: Proxy v -> Int64 -> Key v BackendSpecific
  -- keyToInt :: Key v BackendSpecific -> Int64
  -- intToAuto :: Proxy v -> Int64 -> AutoKey v
  -- autoToInt :: Proxy v -> AutoKey v -> Int64

  ------------------------------------------------------------------------
  crudGet :: Key v BackendSpecific -> (Handler App App) v
  crudGet = getEntity

  ------------------------------------------------------------------------
  handleGet :: Proxy v -> Handler App App ()
  handleGet p = do
    getId <- getParam "id"
    case getId of
      Nothing ->
        json =<< (runGH selectAll :: Handler App App [(AutoKey v, v)])
      Just s  ->
        exceptT err300 json $ do
          k <- hoistEither . note "Bad id parse" . readMay $ B8.unpack s
          lift $ crudGet (intToKey p k :: Key v BackendSpecific)

  ------------------------------------------------------------------------
  crudPost :: v -> Handler App App (Key v BackendSpecific)
  crudPost = postEntity

  ------------------------------------------------------------------------
  handlePost :: Proxy v -> Handler App App ()
  handlePost _ = exceptT err300 json $ do
    (v :: v) <- ExceptT $ A.eitherDecode <$> readRequestBody 100000
    (k :: Key v BackendSpecific) <- ExceptT . fmap Right $ postEntity v
    return k

  ------------------------------------------------------------------------
  crudPut :: Key v BackendSpecific -> v -> Handler App App ()
  crudPut = putEntity

  ------------------------------------------------------------------------
  handlePut :: Proxy v -> Handler App App ()
  handlePut _ =
    exceptT err300 (const $ return ()) $ do
      (putId :: Key k BackendSpecific) <-
        (hoistEither . note "Bad id parse" . readMay . B8.unpack)
        =<< (ExceptT . fmap (note "No id param") $ getParam "id")
      (v :: v) <- ExceptT $ A.eitherDecode <$> readRequestBody 1000000
      ExceptT . fmap Right $ putEntity putId v

  crudDelete :: Key v BackendSpecific -> Handler App App Bool
  crudDelete = deleteEntity

  ------------------------------------------------------------------------
  handleDelete :: Proxy v -> Handler App App ()
  handleDelete _ = exceptT err300 (const $ return ()) $ do
    (delId :: Key v BackendSpecific) <-
       (hoistEither . note "Bad id parse" . readMay . B8.unpack)
       =<< (ExceptT . fmap (note "No id param") $ getParam "id")
    ExceptT . fmap Right $ crudDelete delId

  ------------------------------------------------------------------------
--   crudRoutes :: Typeable v
--              => Proxy v
--              -> [(B8.ByteString, Handler App App ())]
--   crudRoutes p =
--     let tName = B8.pack $ show (typeRep p)
--         andId = (<> "/:id")
--     in  [(andId tName , method GET    (handleGet p))
--         ,(tName <> "s", method GET    (handleGet p))
--         ,(tName,        method POST   (handlePost p))
--         ,(tName,        method PUT    (handlePut p))
--         ,(tName,        method DELETE (handleDelete p))
--         ]

------------------------------------------------------------------------------
getEntity :: (PersistEntity a, PrimitivePersistField (Key a BackendSpecific))
             => Key a BackendSpecific
             -> Handler App App a
getEntity k = maybe (err300 "Bad lookup") return =<< runGH (get k)

------------------------------------------------------------------------------
getAllEntities :: (PersistEntity a, PrimitivePersistField (Key a BackendSpecific))
               => Proxy a
               -> Handler App App [(AutoKey a, a)]
getAllEntities _ = runGH selectAll

------------------------------------------------------------------------------
postEntity :: forall a.(PersistEntity a, Crud a) => a -> Handler App App (Key a BackendSpecific)
postEntity u = method POST $ do
  assertRole [Admin, Researcher]
  k <- runGH (insert u)
  return (intToKey Proxy $ autoToInt (Proxy :: Proxy a) k)

------------------------------------------------------------------------------
putEntity :: (PersistEntity a, PrimitivePersistField (Key a BackendSpecific))
          => Key a BackendSpecific
          -> a
          -> Handler App App ()
putEntity k u = do
  assertRole [Admin, Researcher]
  method PUT $ runGH (replace k u)


------------------------------------------------------------------------------
deleteEntity
  :: (PersistEntity a, PrimitivePersistField (Key a BackendSpecific))
  => Key a BackendSpecific
  -> Handler App App Bool
deleteEntity k = do
  assertRole [Admin]
  u <- runGH (get k)
  runGH (deleteBy k)
  return (isJust u)

crudServer :: Crud v => Proxy v -> Server (CrudAPI v) AppHandler
crudServer p =
  getServer p :<|> getsServer p :<|> postServer p :<|> putServer :<|> deleteServer p


type CrudAPI a = GetAPI a :<|> GetsAPI a
                 :<|> PostAPI a :<|> PutAPI a :<|> DeleteAPI a

type GetAPI a  = Capture "id" Int64 :> Get '[JSON] a
type GetsAPI a = Get '[JSON] [(Int64, a)]
type PostAPI a = ReqBody '[JSON] a :> Post '[JSON] Int64
type PutAPI  a = Capture "id" Int64 :> ReqBody '[JSON] a :> Put '[JSON] ()
type DeleteAPI a = Capture "id" Int64 :> Delete '[JSON] Bool


getServer :: Crud v => Proxy v -> Server (GetAPI v) AppHandler
getServer p k = crudGet (intToKey p k)

getsServer :: Crud v => Proxy v -> Server (GetsAPI v) AppHandler
getsServer p = do
  entPairs <- getAllEntities p
  return $ map (first (autoToInt p)) entPairs

postServer :: forall v.Crud v => Proxy v -> Server (PostAPI v) AppHandler
postServer _ v = do
  k <- postEntity v
  return (keyToInt k)

putServer :: Crud v => Server (PutAPI v) AppHandler
putServer i v = do
  let k  = intToKey Proxy i
  liftIO $ print "HELLO"
  putEntity k v

deleteServer :: Crud v => Proxy v -> Server (DeleteAPI v) AppHandler
deleteServer p i = deleteEntity (intToKey p i)

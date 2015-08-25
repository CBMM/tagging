{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeFamilies   #-}


module Tagging.Crud where

import           Data.Functor
import qualified Data.Aeson as A
import           Data.Foldable
import           Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import GHC.Int
import Reflex.Dom
import Reflex.Dom.Xhr
import Reflex.Dynamic.TH

import Tagging.User
import Tagging.Stimulus
import Tagging.Response

class (A.FromJSON v, A.ToJSON v) => Crud v where

  resourceName :: Proxy v -> String

  inputWidget :: MonadWidget t m => Proxy v -> m (Dynamic t v) -- TODO: Proxy is needed?

  getEntity :: MonadWidget t m => Proxy v -> Event t Int64 -> m (Event t (Maybe v))
  getEntity p ks = getAndDecode (fmap toApi ks)
      where toApi k = ("api/" <> resourceName p <> "/" <> show k)

  getAllEntities :: MonadWidget t m => Proxy v -> Event t () -> m (Event t (Map Int64 v))
  getAllEntities p triggers = do
    mJson <- getAndDecode (triggers $> "api/" <> resourceName p)
    return $ (Map.fromList . toList) <$> mJson

  postEntity :: MonadWidget t m => Event t v ->  m (Event t Int64)

  putEntity :: MonadWidget t m => Event t (Int64,v) -> m (Event t ())

  deleteEntity :: MonadWidget t m => Proxy v -> Event t Int64 -> m (Event t ())


crudTableWidget :: forall t m v.(MonadWidget t m, Crud v)
  => Proxy v
  -> Dynamic t (v -> Bool)
  -> m (Dynamic t (Map Int64 v))
crudTableWidget p dynValidate = mdo

  vMap <- holdDyn mempty =<< getAllEntities p (crudEvents p rowEvents)

  rowEvents <- elClass "table" "crud-table" $ do
    --newRowEvents      <- newRowWidget p
    existingRowEvents <- list vMap crudRowWidget
    --return (newRowEvents <> existingRowEvents)
    return existingRowEvents

  return vMap

crudEvents :: MonadWidget t m
           => Proxy v
           -> CrudRowEvents t m v
           -> m (Event t ())
           -- -> m (Event t (Map Int64 v -> Map Int64 v)) -- this is harder!
           --   Think I need Doug's library to associate requests with
           --   responses
crudEvents p es = do
  postResps <- performRequestAsync $ ffor (postEvent es) $ \e ->
                 XhrRequest "POST" ("/api/" <> resourceName p)
                 def {_xhrRequestConfig_sendData = Just (A.encode e)}
  putResps  <- performRequestAsync $ ffor (putEvent es) $ \(k,e) ->
                 XhrRequest "PUT" ("/api/" <> resourceName p <> "/" <> show k)
                 def {_xhrRequestConfig_sendData = Just (A.encode e)}
  delResps  <- performRequestAsync $ ffor (delEvent es) $ \k ->
                 XhrRequest "DELETE" ("/api" <> resourceName p <> "/" show k)
                 def
  return $ (() <$ postResps)
           (() <$ putResps)
           (() <$ delResps)

data CrudRowEvents t (m :: * -> *) v =
  CrudRowEvents { delEvent  :: Event t Int64
                , postEvent :: Event t v
                , putEvent  :: Event t (Int64,v)
}


instance Monoid (CrudRowEvents t m v) where
  mempty        = CrudRowEvents mempty mempty mempty
  a `mappend` b = CrudRowEvents
                    (delEvent  a <> delEvent  b)
                    (postEvent a <> postEvent b)
                    (putEvent  a <> putEvent  b)


crudRowWidget :: (MonadWidget t m, Crud v)
              => (Int64 -> Dynamic t v -> m (Event t (CrudRowEvents t m v)))
              -> m (Event t (CrudRowEvents t m v))
crudRowWidget dynIntV = do
  text "RowWidget" >> return undefined


instance Crud StimulusResource where
  resourceName _ = "StimulusResource"
  inputWidget  _ = do
    text "F1:"
    f1 <- fmap T.pack $ _textInput_value =<< textInput def
    text "F2:"
    f2 <- fmap T.pack $ _textInput_value =<< textInput def
    text "F3:"
    f3 <- fmap T.pack $ _textInput_value =<< textInput def
    return $ $( qDyn [| StimulusResource $(unqDyn [|f1|]) $(unqDyn [|f2|]) $(unqDyn [|f3|]) |] )


instance Crud TaggingUser where
  resourceName _ = "TaggingUser"
  inputWidget  _ = undefined

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
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
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


-----------------------------------------------------------------------------
crudTableWidget :: forall t m v.(MonadWidget t m, Crud v)
  => Proxy v
  -> Dynamic t (v -> Bool)
  -> m (Event t (Map Int64 (CrudRowCmds v)))
crudTableWidget p dynValidate = mdo
  updateEvents <- crudEvents p rowEvents
  vMap <- holdDyn mempty =<< getAllEntities p (() <$ updateEvents)

  rowEvents <- elClass "table" "crud-table" $ do
    --newRowEvents      <- newRowWidget p
    existingRowEvents <- listViewWithKey vMap crudRowWidget
    --return (newRowEvents <> existingRowEvents)
    return existingRowEvents

  return rowEvents

-----------------------------------------------------------------------------
crudEvents :: MonadWidget t m
           => Proxy v
           -- -> CrudRowEvents t m v
           -> Event t (CrudRowCmds v)
           -> m (Event t (CrudRowCmds v))
           -- -> m (Event t (Map Int64 v -> Map Int64 v)) -- this is harder!
           --   Think I need Doug's library to associate requests with
           --   responses
crudEvents p es = ffor es $ \c -> do
  postResps <- case crudPost c of
    Nothing -> never
    Just v  -> performRequestAsync $
                 XhrRequest "POST" ("/api/" <> resourceName p)
                 def {_xhrRequestConfig_sendData = Just (BS.unpack $ A.encode v)}
  putResps  <- case crudPut c of
    Nothing    -> never
    Just (k,v) -> performRequestAsync $
                    XhrRequest "PUT" ("/api/" <> resourceName p <> "/" <> show k)
                    def {_xhrRequestConfig_sendData = Just (BS.unpack $ A.encode v)}
  delResps  <- case crudDel c of
    Nothing -> never
    Just k  -> performRequestAsync $ ffor (crudDel c) $ \k ->
                 XhrRequest "DELETE" ("/api" <> resourceName p <> "/" show k)
                 def

  return es
  --return $ leftmost [(() <$ postResps)
  --                  ,(() <$ putResps)
  --                  ,(() <$ delResps)

data CrudRowCmds v = CrudRowCmds {
    crudDel  :: Maybe Int64
  , crudPost :: Maybe v
  , crudPut  :: Maybe (Int64, v)
  }
--
-- data CrudRowEvents t (m :: * -> *) v =
--   CrudRowEvents { delEvent  :: Event t Int64
--                 , postEvent :: Event t v
--                 , putEvent  :: Event t (Int64,v)
-- }
--
--
-- instance Monoid (CrudRowEvents t m v) where
--   mempty        = CrudRowEvents mempty mempty mempty
--   a `mappend` b = CrudRowEvents
--                     (delEvent  a `mappend` delEvent  b)
--                     (postEvent a `mappend` postEvent b)
--                     (putEvent  a `mappend` putEvent  b)
--

crudRowWidget :: (MonadWidget t m, Crud v)
              => (Int64 -> Dynamic t v -> m (CrudRowCmds v))
              -> m (Event t (CrudRowCmds v))
crudRowWidget dynIntV = do
  text "RowWidget" >> return undefined


instance Crud StimulusResource where
  resourceName _ = "StimulusResource"
  inputWidget  _ = do
    text "F1:"
    f1 <- _textInput_value <$> textInput def
    text "F2:"
    f2 <- _textInput_value <$> textInput def
    text "F3:"
    f3 <- _textInput_value <$> textInput def
    return $ $( qDyn [| StimulusResource
                        (T.pack $(unqDyn [|f1|]))
                        (T.pack $(unqDyn [|f2|]))
                        (T.pack $(unqDyn [|f3|])) |] )


instance Crud TaggingUser where
  resourceName _ = "TaggingUser"
  inputWidget  _ = undefined

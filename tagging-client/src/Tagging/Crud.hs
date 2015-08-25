{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Tagging.Crud where

import qualified Data.Aeson as A
import Data.Monoid
import Data.Proxy
import GHC.Int
import Reflex.Dom
import Reflex.Dom.Xhr

import Tagging.User
import Tagging.Stimulus
import Tagging.Response

class (A.FromJSON v, A.ToJSON v) => Crud v where

  resourceName :: Proxy v -> String

  inputWidget :: MonadWidget t m => Proxy v -> m (Dynamic t v) -- TODO: Proxy is needed?

  getEntity :: MonadWidget t m => Proxy v -> Event t Int64 -> m (Event t (Maybe v))
  getEntity p ks = getAndDecode (fmap toApi ks)
      where toApi k = ("api/"
                       <> resourceName p
                       <> "/"
                       <> show k)

  getAllEntities :: MonadWidget t m => Event t () -> m (Event t [(Int64,v)])

  postEntity :: MonadWidget t m => Event t v ->  m (Event t Int64)

  putEntity :: MonadWidget t m => Event t (Int64,v) -> m (Event t ())

  deleteEntity :: MonadWidget t m => Proxy v -> Event t Int64 -> m (Event t ())


crudTableWidget :: forall t m v.(MonadWidget t m, Crud v)
  => Proxy v
  -> Dynamic t (v -> Bool)
  -> m (Dynamic t [v])
crudTableWidget p dynValidate = mdo

  submitButton <- button "Add"
  newEntityDyn <- inputWidget p
  isOk         <- combineDyn ($) dynValidate newEntityDyn

  vs           <- holdDyn [] =<< (getAllEntities submitButton)
  elClass "table" "crud-table" $ simpleList vs crudRowWidget

crudRowWidget :: (MonadWidget t m, Crud v) => Dynamic t (Int64,v) -> m v
crudRowWidget dynIntV = text "RowWidget" >> return undefined

instance Crud StimulusResource where
  resourceName _ = "StimulusResource"
  inputWidget  _ = StimulusResource "Test" "test" "test"

instance Crud TaggingUser where
  resourceName _ = "TaggingUser"
  inputWidget  _ = undefined

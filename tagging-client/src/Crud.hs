{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE OverloadedStrings #-}

module Tagging.Crud where

import Reflex.Dom

class (FromJSON v, ToJSON v,
      PersistEntity v,
      PrimitivePersistField (Key v BackendSpecific),
      A.ToJSON (Key v BackendSpecific)
      A.FromJSON (Key v BackendSpecific)
      ) => Crud v where

  inputWidget :: MonadWidget t m => Proxy v -> m (Dynamic t v) -- TODO: Proxy is needed?

  getEntity :: MonadWidget t m => Key v BackendSpecific -> m (Event v)

  getAllEntities :: MonadWidget t m => m (Event [v])

  postEntity :: MonadWidget t m => v -> m (Event ())

  putEntity :: MonadWidget t m => Key v BackendSpecific -> v -> m (Event ())

  deleteEntity :: MonadWidget t m => Key v BackendSpeficic -> m (Event ())


crudTableWidget :: (MonadWidget t m, Crud v)
  => Proxy v
  -> Dynamic t (v -> Bool)
  -> m (Dynamic t [v])
crudTableWidget dynElems validate = mdo

  submitButton <- button "Add"
  newEntityDyn <- inputWidget
  isOk         <- combineDyn validate newEntityDyn

  vs           <- holdDyn [] =<< (getAllEntities)
  elClass "table" "crud-table" $ listViewWithKey vs crudRowWidget

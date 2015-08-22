module Tagging.Crud where

import Reflex.Dom

class (FromJSON v, ToJSON v,
      PersistEntity v,
      PrimitivePersistField (Key v BackendSpecific),
      A.ToJSON (Key v BackendSpecific)
      A.FromJSON (Key v BackendSpecific)
      ) => Crud v where



{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.Database where

import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Data.Maybe (fromMaybe, maybeToList)
import Data.Proxy
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import Database.Groundhog
import Database.Groundhog.Core
import Database.Groundhog.Generic
import Database.Groundhog.TH
import GHC.Int
import Tagging.Stimulus
import Tagging.User
import Tagging.Response


mkPersist defaultCodegenConfig [groundhog|
definitions:
  - entity: StimulusResource
  - entity: StimulusSequence
    keys:
      - name: SsName
    constructors:
      - name: StimulusSequence
        uniques:
          - name: SsName
            fields: [ssName]
  - entity: StimSeqItem
|]

mkPersist defaultCodegenConfig [groundhog|
  - primitive: Role
    representation: showread
  - entity: TaggingUser
    keys:
      - name: TuId
    constructors:
      - name: TaggingUser
        uniques:
          - name: TuId
            fields: [tuId]
|]

instance PersistField A.Value where
  persistName _     = "json"
  toPersistValues   = primToPersistValue . A.encode
  fromPersistValues = primFromPersistValue
  dbType _ _        = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField A.Value where
  toPrimitivePersistValue p v   = toPrimitivePersistValue p $ A.encode v
  fromPrimitivePersistValue p s = fromMaybe A.Null
                                  (A.decode $ fromPrimitivePersistValue p s)

mkPersist defaultCodegenConfig [groundhog|
  - entity: StimulusRequest
  - entity: StimulusResponse
|]

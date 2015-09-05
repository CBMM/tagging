{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Server.Database where

import Control.Monad.IO.Class
import qualified Data.Aeson as A
import Data.Proxy
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import Database.Groundhog
import Database.Groundhog.Core
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
  - entity: TaggingUser
    keys:
      - name: TuId
    constructors:
      - name: TaggingUser
        uniques:
          - name: TuId
            fields: [tuId]
  - entity: Role
    constructors:
      - name: Admin
      - name: Subject
      - name: Researcher
|]


mkPersist defaultCodegenConfig [groundhog|
  - entity: StimulusRequest
  - entity: StimulusResponse
|]

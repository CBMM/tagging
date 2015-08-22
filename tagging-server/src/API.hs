{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module API where

import qualified Data.Aeson         as A
import qualified Data.ByteString    as B8
import qualified Data.Text          as T
import           Database.Groundhog
import           GHC.Int
import           Servant
import           Servant.Server
import           Tagging.User
import           Tagging.Stimulus
import           Tagging.Response
import           Server.Application
import           Server.Crud
import           Server.Session

------------------------------------------------------------------------------
type TaggingAPI =
  SessionAPI
  :<|> SubjectAPI
  :<|> ResourcesAPI
  :<|> "docs" :> Raw AppHandler (AppHandler ())

------------------------------------------------------------------------------
apiProxy :: Proxy TaggingAPI
apiProxy = Proxy

-- TODO: These all end in an http verb. Is that necessary?
------------------------------------------------------------------------------
type ResourcesAPI =
       "tagginguser"      :> CrudAPI TaggingUser
  :<|> "stimulusresource" :> CrudAPI StimulusResource
  :<|> "stimulussequence" :> CrudAPI StimulusSequence
  :<|> "stimseqitem"      :> CrudAPI StimSeqItem
  :<|> "stimulusresponse" :> CrudAPI StimulusResponse


------------------------------------------------------------------------------
type SubjectAPI = "resource" :> Get '[JSON] StimulusResource
             :<|> "response" :> ReqBody '[JSON] StimulusResponse :> Post '[JSON] ()


------------------------------------------------------------------------------
type ResearcherAPI = "assignStart" :> Capture "id" Int :> Put '[JSON] ()


------------------------------------------------------------------------------
type AdminAPI = "assignRole" :> QueryParam "id"     Int
                             :> QueryParam "role"   Role
                             :> QueryParam "revoke" Bool
                             :> Put '[JSON] ()

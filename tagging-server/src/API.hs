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
import           Server.Resources
import           Server.Session
import           Server.Subject

------------------------------------------------------------------------------
type TaggingAPI =
  SessionAPI
  :<|> SubjectAPI
  :<|> ResourcesAPI
  :<|> "docs" :> Raw AppHandler (AppHandler ())


------------------------------------------------------------------------------
type ResearcherAPI = "assignStart" :> Capture "id" Int :> Put '[JSON] ()

                :<|> "loadSequence" :> ReqBody '[JSON] StimulusSequence
                                    :> Post '[JSON] ()


------------------------------------------------------------------------------
type AdminAPI = "assignRole" :> QueryParam "id"     Int
                             :> QueryParam "role"   Role
                             :> QueryParam "revoke" Bool
                             :> Put '[JSON] ()

------------------------------------------------------------------------------
apiProxy :: Proxy TaggingAPI
apiProxy = Proxy

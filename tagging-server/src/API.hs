{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module API where

import qualified Data.Aeson         as A
import qualified Data.ByteString    as B8
import qualified Data.Text          as T
import           Database.Groundhog
import           GHC.Int
import           Servant
import           Servant.Server
import           Servant.Docs
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
  :<|> ResearcherAPI
--   :<|> "docs" :> Raw AppHandler (AppHandler ())
--   :<|> "clients" :> ClientLibs

------------------------------------------------------------------------------
type ResearcherAPI = "assignsequence" :> Capture "userid"   Int64
                                      :> Capture "sequence" Int64
                                      :> Put '[JSON] ()

                :<|> "loadSequence" :> ReqBody '[JSON] (StimulusSequence,
                                                        [StimSeqItem])
                                    :> Post '[JSON] Int64

                :<|> "subjectdata"  :> Capture "userid"     Int64
                                    :> Capture "sequence" Int64
                                    :> Get '[JSON] (Headers
                                                    '[Header "Content-Disposition" String]
                                                    [StimulusResponse])
                :<|> "sequence" :> Capture "sequence" Int64
                                :> Get '[JSON] (StimulusSequence,
                                                [StimSeqItem])


------------------------------------------------------------------------------
type AdminAPI = "assignRole" :> QueryParam "id"     Int
                             :> QueryParam "role"   Role
                             :> QueryParam "revoke" Bool
                             :> Put '[JSON] ()


type ClientLibs = "matlab"     :> Raw
             :<|> "javascript" :> Raw


------------------------------------------------------------------------------
apiProxy :: Proxy TaggingAPI
apiProxy = Proxy

instance ToCapture (Capture "sequence" Int64) where
  toCapture _ = DocCapture "sequence" "Key for the Stimulus Sequence"

instance ToCapture (Capture "userid" Int64) where
  toCapture _ = DocCapture "sequence" "Numeric key for the user"

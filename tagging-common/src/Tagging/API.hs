{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Tagging.API where

import qualified Data.Aeson         as A
import qualified Data.Aeson.Types   as A
import qualified Data.ByteString    as B8
import           Data.Char          (toLower)
import           Data.Proxy         (Proxy(..))
import qualified Data.Text          as T
import           Database.Groundhog
import           GHC.Generics
import           GHC.Int
import           Servant.API
import           Servant.API.Capture
-- import           Servant.Docs
import           Tagging.User
import           Tagging.Stimulus
import           Tagging.Response
-- import           Server.Application
-- import           Server.Crud
-- import           Server.Resources
-- import           Server.Session
-- import           Server.Subject

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

------------------------------------------------------------------------------
type SubjectAPI = "currentstim"       :> Get '[JSON] StimSeqItem
             :<|> "currentsequence"   :> Get '[JSON] StimulusSequence
             :<|> "currentassignment" :> Get '[JSON] Assignment
             :<|> "fullposinfo"       :> Get '[JSON] (Maybe
                                         (Assignment,
                                          StimulusSequence,
                                          StimSeqItem))
             :<|> "progress"          :> Get '[JSON] Progress
             :<|> "response"          :> QueryFlag "advance" :> ReqBody '[JSON] ResponsePayload
                                      :> Post '[JSON] ()
             :<|> "answerkey"         :> QueryParam "experiment" Int
                                      :> Get '[JSON] [StimSeqAnswer]

------------------------------------------------------------------------------
type SessionAPI =
--       "login" :> ReqBody '[FormUrlEncoded, JSON] LoginInfo
--               :> Raw AppHandler (AppHandler ())

   "currentuser" :> Get '[JSON] TaggingUser

--  :<|> "newuser" :> ReqBody '[FormUrlEncoded, JSON] RegisterInfo
--                 :> Post '[JSON] ()

--  :<|> "logout" :> Get '[JSON] ()



data LoginInfo = LoginInfo {
    liUsername :: T.Text
  , liPassword :: T.Text
  , liRemember :: Bool
  } deriving (Eq, Show, Generic)

instance A.ToJSON LoginInfo where
  toJSON = A.genericToJSON A.defaultOptions { A.fieldLabelModifier = drop 2 . map toLower }

instance A.FromJSON LoginInfo where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = drop 2 . map toLower}

data RegisterInfo = RegisterInfo {
    riUsername :: T.Text
  , riPassword :: T.Text
} deriving (Eq, Show, Generic)

instance A.ToJSON RegisterInfo where
  toJSON = A.genericToJSON A.defaultOptions { A.fieldLabelModifier = drop 2 . map toLower }

instance A.FromJSON RegisterInfo where
  parseJSON = A.genericParseJSON A.defaultOptions { A.fieldLabelModifier = drop 2 . map toLower}


------------------------------------------------------------------------------
type ResourcesAPI =
       "tagginguser"      :> CrudAPI TaggingUser
  :<|> "assignment"       :> CrudAPI Assignment
  :<|> "stimulussequence" :> CrudAPI StimulusSequence
  :<|> "stimseqitem"      :> CrudAPI StimSeqItem
  :<|> "stimulusresponse" :> CrudAPI StimulusResponse
  :<|> "stimulusrequest"  :> CrudAPI StimulusRequest

type CrudAPI a = GetAPI a :<|> GetsAPI a
                 :<|> PostAPI a :<|> PutAPI a :<|> DeleteAPI a

type GetAPI a  = Capture "id" Int64 :> Get '[JSON] a
type GetsAPI a = Get '[JSON] [(Int64, a)]
type PostAPI a = ReqBody '[JSON] a :> Post '[JSON] Int64
type PutAPI  a = Capture "id" Int64 :> ReqBody '[JSON] a :> Put '[JSON] ()
type DeleteAPI a = Capture "id" Int64 :> Delete '[JSON] Bool


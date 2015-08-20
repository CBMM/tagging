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


-- TODO: These all end in an http verb. Is that necessary?
------------------------------------------------------------------------------

type CrudAPI a = GetAPI a :<|> GetsAPI a
                 :<|> PostAPI a :<|> PutAPI a :<|> DeleteAPI a

type GetAPI a  = Capture "id" Int64 :> Get '[JSON] a
type GetsAPI a = Get '[JSON] [a]
type PostAPI a = ReqBody '[JSON] a :> Post '[] Int64
type PutAPI  a = Capture "id" Int64 :> ReqBody '[JSON] a :> Put '[] ()
type DeleteAPI a = Capture "id" Int64 :> Delete '[] Bool

type ResourcesAPI =
       "tagginguser"      :> CrudAPI TaggingUser
  :<|> "stimulusresource" :> CrudAPI StimulusResource
  :<|> "stimulussequence" :> CrudAPI StimulusSequence
  :<|> "stimseqitem"      :> CrudAPI StimSeqItem
  :<|> "stimulusresponse" :> CrudAPI StimulusResponse


type SessionAPI = --"login"   :> Raw AppHandler (AppHandler ())
                  "newuser" :> QueryParam "username" T.Text
                            :> QueryParam "password" T.Text
                            :> QueryFlag  "remember"
                            :> QueryParam "realname" T.Text
                            :> QueryParam "studentid" T.Text
                            :> Post '[] ()
             :<|> "newuser" :> Raw AppHandler (AppHandler ())
             :<|> "logout"  :> Raw AppHandler (AppHandler ())


type SubjectAPI = "resource" :> Get '[JSON] StimulusResource
             :<|> "response" :> ReqBody '[JSON] A.Value


type ResearcherAPI = "assignStart" :> Capture "id" Int :> Put '[] ()


type AdminAPI = "assignRole" :> QueryParam "id"     Int
                             :> QueryParam "role"   Role
                             :> QueryParam "revoke" Bool
                             :> Put '[] ()


type API = SessionAPI
           :<|> SubjectAPI
           :<|> ResourcesAPI

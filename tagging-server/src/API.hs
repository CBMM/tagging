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

type CrudAPI n a = n :> Capture "id" :> Get    '[JSON] a
              :<|> n :>                 Get    '[JSON] [a]
              :<|> ReqBody '[JSON] a :> Post   '[]     (AutoKey a)
              :<|> ReqBody '[JSON] a :> Put    '[]     ()
              :<|> Capture "id"      :> Delete '[]     ()


type SessionAPI = "login"   :> Raw AppHandler (AppHandler ())
             :<|> "newuser" :> QueryParam "username" T.Text
                            :> QueryParam "password" T.Text
                            :> QueryFlag  "remember"
                            :> QueryParam "realname" T.Text
                            :> QueryParam "studentid" T.Text
                            :> Post '[] Int64
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
           :<|> CrudAPI "tagginguser"      TaggingUser
           :<|> CrudAPI "stimulusresource" StimulusResource
           :<|> CrudAPI "stimulussequence" StimulusSequence
           :<|> CrudAPI "stimseqitem"      StimSeqItem
           :<|> CrudAPI "stimulusresponse" StimulusResponse

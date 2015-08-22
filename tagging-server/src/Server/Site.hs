{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE DeriveGeneric       #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Server.Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Error
import           Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Map.Syntax ((##))
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Generics
import           GHC.Int
import           Servant
import           Servant.Docs
import           Servant.Server
import           Servant.Server.Internal.SnapShims
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Groundhog.Postgresql
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Server.Utils
import           Tagging.User
import           Tagging.Stimulus
import           Tagging.Response
------------------------------------------------------------------------------
import           API
import           APIDocs
import           Server.Application
import           Server.Crud
import           Server.Experimenter
import           Server.Resources
import           Server.Session
import           Server.Subject

apiServer :: Server TaggingAPI AppHandler
apiServer = sessionServer :<|> subjectServer
            :<|> resourceServer :<|> docsServer


apiApplication :: Application AppHandler
apiApplication = serve apiProxy apiServer

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("login",    handleLoginSubmit)
         --, ("logout",   handleLogout)
         --, ("new_user", handleNewUser)
         , ("all_users", getAllUsers >>= json)

         -- Experimenter routes
         --, ("asasign_seq_start", assignUserSeqStart)

         --, ("getCurrentStimulus", getCurrentStimulusResource)
         --, ("submitResponse",     handleSubmitResponse)
         , ("api", applicationToSnap apiApplication)
         --, ("/", with auth $ handleLogin Nothing)
         , ("migrateResources", migrateResources)
         , ("",          Snap.Util.FileServe.serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do

    h <- nestSnaplet "" heist $ heistInit "templates"

    d <- nestSnaplet "" db pgsInit

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    a <- nestSnaplet "auth" auth $
           initPostgresAuth sess d

    g <- nestSnaplet "gh" gdb initGroundhogPostgres

    addRoutes routes
    addAuthSplices h auth
    return $ App h s a d g


docsServer = lift . writeBS . BS.pack . markdown $ docsWithIntros [docsIntro] apiProxy

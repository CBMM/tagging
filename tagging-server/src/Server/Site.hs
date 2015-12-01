{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8  as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Configurator      as C
import           Data.Map.Syntax ((##))
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Database.Groundhog.Postgresql as GH
import           GHC.Generics
import           GHC.Int
import qualified Heist.Interpreted as I
import           Servant
import           Servant.Docs
import           Servant.Server
import           Servant.Server.Internal.SnapShims
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Heist
import qualified Snap.Snaplet.Heist.Interpreted as I
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
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
            :<|> resourceServer -- :<|> docsServer


apiApplication :: Application AppHandler
apiApplication = serve apiProxy apiServer

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("login",    handleLoginSubmit)
         , ("currentuser", with auth (currentUser >>= writeBS . BS.pack . show))
         --, ("logout",   handleLogout)
         --, ("new_user", handleNewUser)
         , ("all_users", getAllUsers >>= json)
         , ("client/:taskname", handleTaggingClient)
         -- Experimenter routes
         --, ("asasign_seq_start", assignUserSeqStart)

         --, ("getCurrentStimulus", getCurrentStimulusResource)
         --, ("submitResponse",     handleSubmitResponse)
         , ("adminPanel", adminPanel)
         , ("api", applicationToSnap apiApplication)
         --, ("/", with auth $ handleLogin Nothing)
         , ("migrateResources", migrateHandler)
         , ("",          Snap.Util.FileServe.serveDirectory "static")
         ]


handleTaggingClient :: AppHandler ()
handleTaggingClient = do
  taskName <- getParam "taskname"
  case taskName of
    Nothing -> writeBS "hello"
    Just tn ->
      let jsFile  = T.decodeUtf8 $ "/media/js/"  <> tn <> ".jsexe"
          cssFile = T.decodeUtf8 $ "/media/css/" <> tn <> ".css"
      --in  writeText jsFile
      in  I.renderWithSplices "_taggingclient" $ do
            "jsdir" ## I.textSplice jsFile
            "cssfile" ## I.textSplice cssFile

adminPanel :: AppHandler ()
adminPanel = do
  assertRole [Admin, Researcher]
  I.renderWithSplices
    "_adminpanel"
    ("jsdir" ## I.textSplice "/media/js/AdminPanel.jsexe")

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do


    h <- nestSnaplet "" heist $ heistInit "templates"

    d <- nestSnaplet "" db pgsInit

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)

    a <- nestSnaplet "auth" auth $
           initPostgresAuth sess d

    cfg <- C.subconfig "postgresql-simple" <$> getSnapletUserConfig
    connstr <- liftIO $ T.decodeUtf8 <$> getConnectionString cfg
    liftIO $ putStrLn $ "connstr: " <> T.unpack connstr

    g <- liftIO $ GH.withPostgresqlPool (T.unpack connstr) 3 return

    liftIO (print "SnapletUserConfig:")
    cBig <- getSnapletUserConfig
    liftIO $ C.display cBig

    addRoutes routes
    addAuthSplices h auth

    liftIO $ GH.runDbConn migrateResourcesIO g

    --  Touch the session on each interaction to keep the key
    --  from expiring
    wrapSite (\s -> with sess touchSession >> s >> with sess commitSession)

    return $ App h s a d g


--docsServ
--docsServer = writeBS . BS.pack . markdown $ docsWithIntros [docsIntro] apiProxy

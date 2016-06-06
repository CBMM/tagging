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
import           Data.Bifunctor (first)
import qualified Data.ByteString.Char8  as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.Configurator      as C
import           Data.Foldable
import           Data.Map.Syntax ((##))
import           Data.Monoid
import           Data.Proxy
import           Data.Time              (getCurrentTime)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.UUID          as UUID
import qualified Data.UUID.V4       (nextRandom)
import qualified Database.Groundhog.Postgresql as GH
import           GHC.Generics
import           GHC.Int
import qualified Heist.Interpreted as I
import           Servant hiding (GET, POST, PUT, DELETE)
import           Servant.Docs
import           Servant.Server
import           Servant.Server.Internal.SnapShims
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.PostgresqlSimple (getConnectionString)
-- import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
-- import           Snap.Snaplet.Heist
import qualified Snap.Snaplet.Heist as SHeist
import qualified Snap.Snaplet.Heist.Interpreted as I
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Server.Utils
import           Tagging.API
import           Tagging.User
import           Tagging.Stimulus
import           Tagging.Response
------------------------------------------------------------------------------
import           APIDocs
import           Server.Application
import           Server.Crud
import           Server.GroundhogAuth
import           Server.Researcher
import           Server.Resources
import           Server.Session
import           Server.Subject

apiServer :: Server TaggingAPI AppHandler
apiServer = sessionServer :<|> subjectServer
            :<|> resourceServer -- :<|> docsServer
            :<|> researcherServer


apiApplication :: Application AppHandler
apiApplication = serve apiProxy apiServer

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("login",    with auth handleLoginSubmit)
         , ("currentuser", with auth (currentUser >>= writeBS . BS.pack . show))
         , ("atest", with auth (logout))
         , ("logout",   handleLogout)
         , ("new_user", handleNewUser)
         , ("all_users", getAllUsers >>= json)
         , ("client/:taskname", handleTaggingClient)
         -- Experimenter routes

         , ("adminPanel", adminPanel)
         , ("api", with sess touchSession >> applicationToSnap apiApplication)
         , ("migrateResources", migrateHandler)
         , ("api/docs", docsServer)
         -- , ("subjectdata/:seqid/:userid", handleSubjectData)
         -- , ("library/matlab", matlabLibrary)
         -- , ("library/javascript", javascriptLibrary)
         , ("", Snap.Util.FileServe.serveDirectory "static")

         , ("", with auth $ handleLogin Nothing)
         ]


------------------------------------------------------------------------------
readParam :: (MonadSnap m, Read a) => BS.ByteString -> ExceptT String m a
readParam pName = do
  p <- noteT ("No parameter " <> BS.unpack pName) $ MaybeT $ getParam pName
  noteT ("Can't read " <> BS.unpack p <> " in param " <> BS.unpack pName) $
    hoistMaybe (readMay (BS.unpack p))


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

assertKey :: Handler App App ()
assertKey = withRequest $ \h -> do
  let maybeKey = getHeader "Authorization" h
  case maybeKey >>= (textToUuid . T.decodeUtf8) of
    Nothing -> pass -- TODO send a good error code
    Just (u :: UUID.UUID) -> do
      t <- liftIO getCurrentTime
      k <- runGH $ GH.select (KKeyField GH.==. u  GH.&&. t GH.>. KExpiresField)
      case length (k :: [APIKey]) of
        0 -> pass
        otherwise -> return ()


handleNewUser :: Handler App App ()
handleNewUser =
  Snap.Core.method GET runPage <|> Snap.Core.method POST runCreate
  where
    runPage = I.render "_new_user"
    runCreate = exceptT Server.Utils.err300 return $ do

      user <- ExceptT $ fmap (first show) $
              with auth $ registerUser "username" "password"

      uid  <- noteT "No userid" $ hoistMaybe ( readMay . T.unpack . unUid =<< userId user :: Maybe Int64)

      n    <- lift $ runGH $ GH.countAll (undefined :: TaggingUser)
      let roles = if n == 0 then [Admin,Subject] else [Subject]
      _ <- lift $ runGH $ GH.insert (TaggingUser (uid :: Int64) Nothing Nothing roles)
      return ()


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do


    h <- nestSnaplet "" heist $ I.heistInit "templates"
    SHeist.setInterpreted h

    -- d <- nestSnaplet "" gh pgsInit

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)

    cfg <- C.subconfig "postgresql-simple" <$> getSnapletUserConfig
    connstr <- liftIO $ T.decodeUtf8 <$> getConnectionString cfg
    liftIO $ putStrLn $ "connstr: " <> T.unpack connstr

    g <- liftIO $ GH.withPostgresqlPool (T.unpack connstr) 3 return

    -- a <- nestSnaplet "auth" auth $
    --        initPostgresAuth sess d

    a <- nestSnaplet "auth" auth $
           initGroundhogAuth sess g


    liftIO (print "SnapletUserConfig:")
    cBig <- getSnapletUserConfig
    liftIO $ C.display cBig

    addRoutes routes
    addAuthSplices h auth

    liftIO $ GH.runDbConn migrateResourcesIO g

    --  Touch the session on each interaction to keep the key
    --  from expiring
    wrapSite (\s -> with sess touchSession >> s >> with sess commitSession)

    return $ App h s a g


docsServer :: ServerT Raw AppHandler
docsServer = writeBS . BS.pack . markdown $ docsWithIntros [docsIntro] apiProxy



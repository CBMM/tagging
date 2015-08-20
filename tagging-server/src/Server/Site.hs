{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

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
import           GHC.Int
import           Servant
import           Servant.Server
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
import           Server.Application
import           Server.Crud
import           Server.Experimenter
import           Server.Resources
import           Server.Subject

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App App ()
handleLoginSubmit =
    with auth $ loginUser "login" "password" Nothing
                (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
--handleNewUser :: Handler App App ()
--handleNewUser = method GET handleForm <|> method POST handleFormSubmit
handleNewUser :: Server (SessionAPI) AppHandler
handleNewUser =      handleFormSubmit
                :<|> lift handleForm
                :<|> lift (with auth handleLogout)
  where

    handleForm :: AppHandler ()
    handleForm = with auth $ render "new_user"

    handleFormSubmit :: Maybe T.Text
                      -> Maybe T.Text
                      -> Bool
                      -> Maybe T.Text
                      -> Maybe T.Text
                      -> EitherT ServantErr AppHandler ()
    handleFormSubmit (Just uname) (Just pw) rem realNm stId =
      lift $ maybeT (Server.Utils.err300 "New user error") return $ do
        user <- hushT $ EitherT $ with auth $ createUser (uname) (T.encodeUtf8 pw)
        uId   <- hoistMaybe (readMay . T.unpack =<< (unUid <$> userId user))
        lift $ gh $ insert (TaggingUser ((uId :: Int)) stId realNm Nothing [Subject])
        return ()

     -- runMaybeT $ do
     --  AuthUser{..} <- hushT $ EitherT $ with auth $ registerUser "login" "password"
     --  tu <- maybeT err300 (gh . insert) $ TaggingUser
     --        <$> pure (maybe (-1) id (readMay . T.unpack . unUid $ userId))
     --        <*> MaybeT (fmap parseStudentId =<< getParam "studentid")
     --        <*> getParam "realname"
     --        <*> pure Nothing
     --        <*> pure [Subject]
     --  gh $ insert tu
     --  redirect "/"

parseStudentId :: String -> Maybe Int
parseStudentId = readMay . filter (`notElem` ['-',' '])

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("login",    handleLoginSubmit)
         --, ("logout",   handleLogout)
         --, ("new_user", handleNewUser)
         , ("all_users", getAllUsers >>= json)

         -- Experimenter routes
         , ("asasign_seq_start", assignUserSeqStart)

         --, ("getCurrentStimulus", getCurrentStimulusResource)
         , ("submitResponse",     handleSubmitResponse)
         , ("",          Snap.Util.FileServe.serveDirectory "static")
         ] ++ crudRoutes (Proxy :: Proxy TaggingUser)
           ++ crudRoutes (Proxy :: Proxy StimulusSequence)
           ++ crudRoutes (Proxy :: Proxy StimulusResource)
           ++ crudRoutes (Proxy :: Proxy StimSeqItem)
           ++ [("migrateResources", migrateResources)]


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

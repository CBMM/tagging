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
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Map.Syntax ((##))
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text as T
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
import           Server.Application
import           Server.Crud
import           Server.Experimenter
import           Server.Resources
import           Server.User

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("login",    with auth handleLoginSubmit)
         , ("logout",   with auth handleLogout)
         , ("new_user", with auth handleNewUser)
         , ("all_users", getAllUsers >>= json)

         -- Experimenter routes
         , ("asasign_seq_start", assignUserSeqStart)
         , ("",          serveDirectory "static")
         ] ++ crudRoutes (Proxy :: Proxy TaggingUser)
           ++ crudRoutes (Proxy :: Proxy StimSeq)
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

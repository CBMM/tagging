{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
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
import           Application
import           Experimenter
import           Tagging.User
import           Tagging.Stimulus
import           Tagging.Response


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



--
-- getUserSequence :: Handler App App (Either String [StimSeqItem])
-- getUserSequence = do
--   mTaggingUser <- with auth $ runMaybeT getTaggingUser
--   maybe (return (Left "Not logged in")) userSeq mTaggingUser
--     where
--       userSeq :: TaggingUser -> Handler App App (Either String [StimSeqItem])
--       userSeq TaggingUser{..} = with gdb $
--         case tuCurrentStimulus of
--           Nothing -> return $ Left "Unassigned"
--           Just ssiKey -> do
--             maybeSSI <- undefined --get ssiKey
--             case maybeSSI of
--               Nothing -> return $ Left "Bad StimSeqItem lookup"
--               Just StimSeqItem{..} ->
--                 undefined
        -- seqElem         <- fmap (note "Unassigned") (hoistMaybe tuCurrentStimulus)
        -- StimSeqItem{..} <- noteT "Bad StimSeqItem lookup"
        --                    . MaybeT . with gdb $ get seqElem
        -- with gdb $ select (SsNameKey ==. ssiStimSeq)
--
-- getTrial :: Handler App (AuthManager App) ()
-- getTrial = maybeT (serverError "Must be logged in") (\_ -> return ()) $ do
--   u@TaggingUser{..} <- getTaggingUser
--   undefined
--   --writeBS $ BS.pack . show $ u


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("login",    with auth handleLoginSubmit)
         , ("logout",   with auth handleLogout)
         , ("new_user", with auth handleNewUser)
         , ("",          serveDirectory "static")
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

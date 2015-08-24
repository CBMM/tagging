{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Server.Session where

import           Control.Error
import           Control.Monad (mzero)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as A
import           Data.Aeson ((.:), (.=), (.:?), (.!=))
import           Database.Groundhog.Postgresql
import           GHC.Generics
import           GHC.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Heist.Interpreted as I
import           Data.Map.Syntax
import           Servant
import           Servant.Docs
import           Snap.Core       (redirect)
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Groundhog.Postgresql

import           Tagging.User
import           Server.Application
import           Server.Utils

data LoginInfo = LoginInfo {
    liUsername :: T.Text
  , liPassword :: T.Text
  , liRemember :: Bool
  } deriving (Eq, Show, Generic)

instance A.ToJSON LoginInfo where
  toJSON (LoginInfo u p r) =
    A.object [("username" .= u)
             ,("password" .= p)
             ,("remember" .= r)]

instance A.FromJSON LoginInfo where
  parseJSON (A.Object v) = LoginInfo
    <$> v .: "username"
    <*> v .: "password"
    <*> v .:? "remember" .!= True
  parseJSON _ = mzero

instance ToSample LoginInfo LoginInfo where
  toSample _ = Just (LoginInfo "greg" "myPassword" True)

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
sessionServer :: Server SessionAPI AppHandler
sessionServer = apiLogin
                :<|> apiNewuser
                :<|> lift (with auth handleLogout)
  where

    apiLogin li = lift $ do
      with auth $ loginByUsername
                  (liUsername li)
                  (ClearText . T.encodeUtf8 $ liPassword li)
                  (liRemember li)
      return ()

    apiNewuser (Just uname) (Just pw) rem realNm stId =
      lift $ maybeT (Server.Utils.err300 "New user error") return $ do
        user <- hushT $ EitherT $ with auth $ createUser (uname) (T.encodeUtf8 pw)
        uId   <- hoistMaybe (readMay . T.unpack =<< (unUid <$> userId user))
        lift $ gh $ insert (TaggingUser ((uId :: Int64)) stId realNm Nothing [Subject])
        return ()
    apiNewuser Nothing _ _ _ _ = error "Username is required"
    apiNewuser _ Nothing _ _ _ = error "password is required"


------------------------------------------------------------------------------
type SessionAPI = "login"   :> ReqBody '[JSON] LoginInfo
                            :> Raw AppHandler (AppHandler ())
             :<|> "newuser" :> QueryParam "username" T.Text
                            :> QueryParam "password" T.Text
                            :> QueryFlag  "remember"
                            :> QueryParam "realname" T.Text
                            :> QueryParam "studentid" T.Text
                            :> Post '[JSON] ()
             :<|> "logout"  :> Raw AppHandler (AppHandler ())




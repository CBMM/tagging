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
import qualified Data.Aeson.Types as A
import           Data.Char
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

import           Tagging.User
import           Server.Application
import           Server.Utils


------------------------------------------------------------------------------
type SessionAPI =
--       "login" :> ReqBody '[FormUrlEncoded, JSON] LoginInfo
--               :> Raw AppHandler (AppHandler ())

       "currentuser" :> Get '[JSON] TaggingUser

--  :<|> "newuser" :> ReqBody '[FormUrlEncoded, JSON] RegisterInfo
--                 :> Post '[JSON] ()

--  :<|> "logout" :> Raw AppHandler (AppHandler ())



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


instance ToSample LoginInfo where
  toSamples _ = singleSample (LoginInfo "greg" "myPassword" True)

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
    loginUser "username" "password" Nothing
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
sessionServer = -- apiLogin
                -- :<|> apiCurrentUser
  apiCurrentUser
                -- :<|> apiNewUser
                -- :<|> with auth handleLogout
  where

    apiLogin li = do
      with auth $ loginByUsername
                  (liUsername li)
                  (ClearText . T.encodeUtf8 $ liPassword li)
                  (liRemember li)
      return ()

    apiNewUser RegisterInfo{..} = maybeT (Server.Utils.err300 "New user error") return $ do
        user <- hushT $ ExceptT $ with auth $ createUser riUsername (T.encodeUtf8 riPassword)
        uId   <- hoistMaybe (readMay . T.unpack =<< (unUid <$> userId user))
        nUser <- lift $ runGH $ countAll (undefined :: TaggingUser)
        lift $ runGH $ do
          n <- countAll (undefined :: TaggingUser)
          let newRoles = if n == 0 then [Admin] else [Subject]
          insert (TaggingUser (uId :: Int64) Nothing Nothing newRoles)
        return ()

    apiCurrentUser =
      exceptT
             (Server.Utils.err300 . ("apiCurrentUser error: " ++))
             return
             getCurrentTaggingUser


instance ToFormUrlEncoded LoginInfo where
  toFormUrlEncoded LoginInfo{..} = [("username",liUsername)
                                   ,("password",liPassword)
                                   ,("remember", T.pack (show liRemember))]

instance FromFormUrlEncoded LoginInfo where
  fromFormUrlEncoded fs =
    LoginInfo
    <$> note "LoginInfo missing username field" (lookup "username" fs)
    <*> note "LoginInfo missing password field" (lookup "password" fs)
    <*> pure (fromMaybe False (readMay . T.unpack =<< lookup "remember" fs))

instance ToFormUrlEncoded RegisterInfo where
  toFormUrlEncoded RegisterInfo{..} = [("username",riUsername)
                                      ,("password",riPassword)]

instance FromFormUrlEncoded RegisterInfo where
  fromFormUrlEncoded fs =
    RegisterInfo
    <$> note "RegisterInfo missing username field" (lookup "username" fs)
    <*> note "RegisterInfo missing password field" (lookup "password" fs)


instance ToSample RegisterInfo where
  toSamples _ = singleSample $ RegisterInfo "SampleUser" "SamplePassword"

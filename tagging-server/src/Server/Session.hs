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
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import qualified Data.Aeson as A
import           Data.Aeson ((.:), (.=), (.:?), (.!=))
import qualified Data.Aeson.Types as A
import           Data.Char
import qualified Database.Groundhog as G
import           Database.Groundhog.Postgresql
import qualified Data.Map as Map
import           Data.Map.Syntax
import           Data.Monoid ((<>), mempty)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Generics
import           GHC.Int
import           Servant
import           Servant.Docs
import           Snap.Core       (redirect, writeText, getParams)
import           Snap.Snaplet
import           Snap.Snaplet.Auth


import qualified Heist.Interpreted as I
import qualified Snap.Snaplet.Heist.Interpreted as I
-- import Heist
-- import Snap.Snaplet.Heist
-- import Heist.Interpreted
import           Tagging.API
import           Tagging.User
import           Tagging.Stimulus
import           Server.Application
import           Server.Utils
import qualified Utils as Utils


------------------------------------------------------------------------------
-- Listing of paths to experiments indexed by int. TODO this information
--   should be in the StimulusSequence table of the tagging db
experimentPaths :: Map.Map Int (T.Text,T.Text)
experimentPaths = Map.fromList[(4,("Home Alone 2","/client/homealone"))
                              ,(5,("Video Memory","/client/videomemory"))
                              ]

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = do
 k' <- getCurrentTaggingUser'
 case k' of
  Nothing -> I.render "_index"
  Just k -> do
   let userKey = Utils.integralToKey (tuId k) :: DefaultKey TaggingUser
   asgns <- runGH $ select $ (AUserField ==. userKey)
   let asgnInfos = catMaybes $
        (\asgn -> Map.lookup (Utils.keyToIntegral $ aSequence asgn)
                  experimentPaths) <$> asgns

   I.renderWithSplices "_index" (asgnsSplices asgnInfos)
   where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err
    asgnSplices (txt, href) = do
      "link" ## I.textSplice href
      "assignmentLabel" ## I.textSplice txt
    bindAsgns = I.mapSplices $ I.runChildrenWith . asgnSplices
    asgnsSplices asgns = errs <> ("assignments" ## (bindAsgns asgns))
    -- asgnsSplices' _ = do
    --   "assignments" ## I.textSplice "HI!"
    --   "atest" ## I.textSplice "Test complete"

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
handleLogout :: Handler App App ()
handleLogout = with auth $ logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
--handleNewUser :: Handler App App ()
--handleNewUser = method GET handleForm <|> method POST handleFormSubmit
sessionServer :: Server SessionAPI AppHandler
sessionServer = handleCurrentTaggingUser
           :<|> handleTurk
  where

    handleTurk (turkUserId) (turkExpId) (extraData) = do
      ps <- getParams
      return $ show ps
      -- TODO real handleTurk implementation:
      --    If turkUserId has previously visited, look up his/her tagging login
      --    Otherwise, make a new tagging login from the turk id
      --    log in
      --    redirect to appropriate page
      -- liftIO . putStrLn . unwords $ [show turkUserId, show turkExpId, show extraData]
      -- return ()

    -- apiLogin li = do
    --   with auth $ loginByUsername
    --               (liUsername li)
    --               (T.encodeUtf8 $ liPassword li)
    --               (liRemember li)
    --   return ()

    -- apiNewUser RegisterInfo{..} = maybeT (Server.Utils.err300 "New user error") return $ do
    --     user <- hushT $ ExceptT $ with auth $ createUser riUsername (T.encodeUtf8 riPassword)
    --     uId   <- hoistMaybe (readMay . T.unpack =<< (unUid <$> userId user))
    --     nUser <- lift $ runGH $ countAll (undefined :: TaggingUser)
    --     lift $ runGH $ do
    --       n <- countAll (undefined :: TaggingUser)
    --       let newRoles = if n == 0 then [Admin] else [Subject]
    --       insert (TaggingUser (uId :: Int64) Nothing Nothing newRoles)
    --     return ()

    -- apiCurrentUser =
    --   exceptT
    --          (Server.Utils.err300 . ("apiCurrentUser error: " ++))
    --          return
    --          getCurrentTaggingUser

-- turk :: Int -> Int -> String -> Handler App App ()
-- turk turkId turkExpId extraData = do
--   let taggingId = turkIdToTaggingLogin turkId
--       taggingPw = T.encodeUtf8 taggingId <> "pass" -- TODO fix this. Use token?
--   isRepeatVisit <- usernameExists taggingId
--   authUser <- case isRepeatVisit of
--     False -> createUser taggingId taggingPw
--     True -> loginUser taggingId taggingPw
--   redirect somewhere


handleCurrentTaggingUser :: Handler App App TaggingUser
handleCurrentTaggingUser =
  exceptT (Server.Utils.err300 . ("apiCurrentUser error" ++))
  return getCurrentTaggingUser

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

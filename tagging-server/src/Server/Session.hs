{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}

module Server.Session where

import           Control.Error
import           Control.Monad (mzero, when)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSL
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
import qualified Web.ClientSession as W


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
import           Server.Researcher
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

instance FromHttpApiData A.Value where
  parseQueryParam = note "Bad decode" . A.decode
    . BSL.fromStrict . T.encodeUtf8

------------------------------------------------------------------------------
-- | Handle new user form submit
sessionServer :: Server SessionAPI AppHandler
sessionServer = handleCurrentTaggingUser
           :<|> handleTurk


-------------------------------------------------------------------------------
-- TODO: Collect all these Text arguments into a `TurkData` type
{-| Servant-snap handel for accepting a user from Mechanical Turk
    MTurk users have their own 'workerId' from their Amazon login.
    We create an account with the same login name on tagging, and
    generate a password by combining that workerId and our site-key.txt
    secret.
    MTurk workers who already have accounts on tagging are force-logged-in.
    The worker is assigned to a stimulus sequence automatically
|-}
handleTurk :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> Maybe T.Text
           -> Maybe Int64 -> Maybe Int -> Maybe Int -> Maybe T.Text
           -> Handler App App ()
handleTurk (Just assignmentId) (Just hitId) (Just workerId)
           (Just turkSubmitTo) (Just redirectUrl) (Just taggingExptNum)
           rangeStart rangeEnd (Just extraData)  = do

  let fI = fromIntegral
      -- ie: turkSubmitTo == https://www.mturk.com/
      finishURL = turkSubmitTo <> "/mturk/externalSubmit?assignmentId=" <> assignmentId
  turkLogin workerId taggingExptNum (fI <$> rangeStart) (fI <$> rangeEnd) finishURL
  I.renderWithSplices "_turklink" $ do
    "turklink"  ## I.textSplice redirectUrl
    "finishurl" ## I.textSplice finishURL
handleTurk _ _ _ _ _ _ _ _ _ = do
  ps <- getParams
  writeText ("Param problem. Params: " <> T.pack (show ps))
  -- TODO: Improve error message



-------------------------------------------------------------------------------
turkLogin :: T.Text -> Int64 -> Maybe Int64 -> Maybe Int64 -> T.Text -> Handler App App ()
turkLogin workerId taggingExptNum startInd endInd finishURL = do

  -- We'll use the site_key to generate passwords for tagging users
  siteKey <- liftIO $ W.getKey "site_key.txt"
  let pw = maybe
           (error "Error: Bad initialization vector during turk pw creation" :: b)
           (\i -> W.encrypt siteKey i (T.encodeUtf8 workerId))
           (W.mkIV "AAAAAAAAAAAAAAAA" :: Maybe W.IV)

  userId <- with auth $ do
    uExist <- usernameExists workerId
    bool (createTurker pw) (loginTurker workerId pw) uExist

  -- TODO: This is copy-pasted from Researcher.hs
  --       Factor out a function like `defaultSequenceBounds`
  let fI = fromIntegral
      exptKey = Utils.integralToKey taggingExptNum :: Key StimulusSequence BackendSpecific
      userKey = Utils.integralToKey userId :: Key TaggingUser BackendSpecific
  seqInds <- runGH $ map ssiIndex <$>
    select (SsiStimulusSequenceField ==. exptKey)
  let (indMin, indMax) = (minimum seqInds, maximum seqInds)

  nAsgn <- runGH $ count (AUserField     ==. userKey                  &&.
                          ASequenceField ==. exptKey                  &&.
                          AStartField    ==. maybe indMin fI startInd &&.
                          AEndField      ==. maybe indMax fI endInd)

  when (nAsgn == 0) $ assignUserSeqStart
    (Just userId) (Just taggingExptNum) startInd endInd (Just finishURL)

  where

    loginTurker uId pw = do
      au <- loginByUsername workerId pw True
      case au of
        Left _ -> error "login error for turk user"
        Right au' -> do
          -- TODO clean up
          let uid  :: Either String Int64 = note "No userid" $ readMay . T.unpack . unUid $
                (fromMaybe (error "error: no userID") (userId au') :: UserId)
          case uid of
            Left e -> error e
            Right uid' -> return uid'
          -- TODO cleanup repeated use of snap auth user ID decoding
          -- TODO fix this error message (must run in Handler App (AuthManager App))

    createTurker pw = do
      au <- createUser workerId pw :: Handler App (AuthManager App) (Either AuthFailure AuthUser)
      case au of
        -- TODO: improve error handler
        Left _ -> error "Create user error on turk user"
        Right au' -> do
          forceLogin au'
          let uid  :: Either String Int64 = note "No userid" $ readMay . T.unpack . unUid $
                (fromMaybe (error "error: no userID") (userId au') :: UserId)
          case uid of
            Left e -> error e
            Right uid' -> do
              runGH $ insert (TaggingUser uid' Nothing (Just "Some Turk User") [Subject])
              return uid'



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

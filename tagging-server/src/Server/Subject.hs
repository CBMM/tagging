{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Server.Subject where

import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Logger (NoLoggingT)
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import           Data.Proxy
import qualified Data.Text as T
import           Data.Time
import           Database.Groundhog
import           Database.Groundhog.Postgresql (Postgresql)
import           GHC.Generics
import           GHC.Int
import           Servant
import           Servant.Docs
import           Servant.Server
import           Snap.Core
import           Snap.Snaplet
import qualified Data.Aeson as A
import           Snap.Snaplet.Groundhog.Postgresql

import           Tagging.Stimulus
import           Tagging.Response
import           Tagging.User
import           Server.Application
import           Server.Crud
import           Server.Database
import           Server.Resources
import           Server.Utils


------------------------------------------------------------------------------
type SubjectAPI = "resource" :> Get '[JSON] (Int64, StimulusResource)
             :<|> "sequence" :> Get '[JSON] (Int64, StimulusSequence)
             :<|> "posinfo"  :> Get '[JSON] PositionInfo
             :<|> "response" :> ReqBody '[JSON] ResponsePayload
                             :> Post '[JSON] ()


------------------------------------------------------------------------------
subjectServer :: Server (SubjectAPI) AppHandler
subjectServer = resource :<|> sequence :<|> pos :<|> response
  where resource   = lift $ getCurrentStimulusResource
        sequence   = lift $ getCurrentStimulusSequence
        pos        = lift $ getCurrentPositionInfo
        response v = lift $ handleSubmitResponse v

------------------------------------------------------------------------------
-- | Add or revoke roles on a user
assignRoleTo :: AutoKey TaggingUser -> Role -> Bool -> Handler App App ()
assignRoleTo targetKey r b = eitherT Server.Utils.err300 (\_ -> return ()) $ do
  lift $ assertRole [Admin]
  tu <- noteT "Bad user lookup" $ MaybeT $ gh $ get targetKey
  let roles' = (if b then L.union [r] else L.delete r) $ tuRoles tu
  lift $ gh $ replace targetKey (tu {tuRoles = roles'})

handleAssignRoleTo :: Handler App App ()
handleAssignRoleTo = void $ runMaybeT $ do
  userKey   <- return undefined -- TODO
  theRole   <- MaybeT (getParam "role")
  role      <- hoistMaybe (readMay $ B8.unpack theRole)
  theUpDown <- MaybeT (getParam "bool")
  upDown    <- hoistMaybe (readMay $ B8.unpack theUpDown)
  lift $ assignRoleTo userKey role upDown


------------------------------------------------------------------------------
-- | Submit a response. Submission will update the user's current-stimulus
--   field to @Just@ `the next sequence stimulus` if there is one, or to
--   @Nothing@ if the sequence is done
--handleSubmitResponse :: StimulusResponse -> Handler App App ()
handleSubmitResponse :: ResponsePayload -> Handler App App ()
handleSubmitResponse t =
  eitherT Server.Utils.err300 (const $ return ()) $ do

    u        <- getCurrentTaggingUser
    stimKey  <- noteT "No assigned stimulus" $ hoistMaybe (tuCurrentStimulus u)

    thisReq  <- noteT "No request record by user for stimulus"
                $ MaybeT $ fmap listToMaybe $ gh
                $ select $ (SreqUserField ==. tuId u
                           &&. SreqStimSeqItemField ==. stimKey)
                           `orderBy` [Asc SreqTimeField]
    tNow     <- lift $ liftIO getCurrentTime

    stim     <- noteT "Bad stim lookup from response"
                $ MaybeT $ gh $ get (intToKey Proxy stimKey)

    lift . gh $ do
      insert (StimulusResponse (tuId u) stimKey
              (sreqTime thisReq) tNow "sometype" (rpBytes t))
      update [TuCurrentStimulusField =. ssiNextItem stim]
        $ (TuIdField ==. tuId u)



------------------------------------------------------------------------------
getCurrentPositionInfo :: AppHandler PositionInfo
getCurrentPositionInfo = eitherT Server.Utils.err300 return $ do
  t   <- lift $ liftIO getCurrentTime
  u   <- getCurrentTaggingUser
  ssi <- getCurrentStimSeqItem
  ss  <- lift getCurrentStimulusSequence
  sr  <- lift getCurrentStimulusResource
  lift . gh $ insert (StimulusRequest (tuId u) (fst ssi) t)
  lift $ modifyResponse $ Snap.Core.addHeader "Cache-Control" "no-cache"
  return $ PositionInfo ss ssi sr


------------------------------------------------------------------------------
getCurrentStimSeqItem :: EitherT String AppHandler (Int64, StimSeqItem)
getCurrentStimSeqItem = do
  loggedInUser <- getCurrentTaggingUser
  itemKey      <- noteT "No sequence assigned"
                  (hoistMaybe $ tuCurrentStimulus loggedInUser)
  ssi          <- noteT "Bad seq lookup" $ MaybeT $ gh $ get (intToKey Proxy itemKey)
  return (itemKey, ssi)


------------------------------------------------------------------------------
getCurrentStimulusResource :: AppHandler (Int64, StimulusResource)
getCurrentStimulusResource = eitherT Server.Utils.err300 return $ do
  ssi <- fmap snd $ getCurrentStimSeqItem
  sr  <- noteT "Bad resource lookup" $ MaybeT $ gh $
         get (intToKey Proxy $ ssiStimulus ssi)
  return (ssiStimulus ssi, sr)


------------------------------------------------------------------------------
getCurrentStimulusSequence :: AppHandler (Int64, StimulusSequence)
getCurrentStimulusSequence = eitherT Server.Utils.err300 return $ do
  ssi <- fmap snd $ getCurrentStimSeqItem
  ss  <- noteT "Bad sequence lookup" $ MaybeT $ gh $
         get (intToKey Proxy $ ssiStimSeq ssi)
  return (ssiStimSeq ssi, ss)

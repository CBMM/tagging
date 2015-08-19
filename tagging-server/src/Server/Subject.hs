{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module Server.Subject where

import Control.Error
import Control.Monad
import Control.Monad.Trans.Class (lift)
import Control.Monad.Logger (NoLoggingT)
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import Data.Proxy
import Database.Groundhog
import Database.Groundhog.Postgresql (Postgresql)
import GHC.Int
import Servant
import Servant.Server
import Snap.Core
import Snap.Snaplet
import qualified Data.Aeson as A
import Snap.Snaplet.Groundhog.Postgresql

import Tagging.Stimulus
import Tagging.Response
import Tagging.User
import API
import Server.Application
import Server.Crud
import Server.Resources
import Server.Utils

subjectServer :: Server (SubjectAPI) AppHandler
subjectServer = resource :<|> response
  where resource = lift $ getCurrentStimulusResource
        response = undefined -- TODO

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
-- | Request the next stimulus in the user's assigned sequence
--   @Nothing@ return values indicates the sequence is finished,
--   Other sorts of errors are signaled with normal http response codes
handleRequestCurrentStimulus :: Handler App App ()
handleRequestCurrentStimulus = eitherT Server.Utils.err300 json $ do

  TaggingUser{..} <- noteT "TaggingUser retrieval error" getCurrentTaggingUser
  StimSeqItem{..} <- noteT "Problem" $ (MaybeT . gh . get)
                     =<< hoistMaybe tuCurrentStimulus
  noteT "Bad stimulus lookup" $ MaybeT $ gh $ get ssiStimulus


------------------------------------------------------------------------------
-- | Submit a response. Submission will update the user's current-stimulus
--   field to @Just@ `the next sequence stimulus` if there is one, or to
--   @Nothing@ if the sequence is done
handleSubmitResponse :: Handler App App ()
handleSubmitResponse = eitherT Server.Utils.err300 (const $ return ()) $ do

  loggedInUser           <- noteT "No logged in tagging user"
                            getCurrentTaggingUser
  r@StimulusResponse{..} <- (hoistEither . A.eitherDecode)
                            =<< (lift $ readRequestBody 1000000)
  stim                   <- noteT "Bad stim lookup from response"
                            $ MaybeT $ gh $ get srStim
  respUser               <- crudGet srUser

  when (tuId loggedInUser /= tuId respUser)
    (lift $ Server.Utils.err300 "Logged in user / reported user mismatch")

  lift . gh $ do
    insert r
    insert (loggedInUser {tuCurrentStimulus = ssiNextItem stim})

getCurrentStimulusResource :: Handler App App StimulusResource
getCurrentStimulusResource = eitherT Server.Utils.err300 return $ do
  loggedInUser <- noteT "No logged it tagging user" getCurrentTaggingUser
  itemKey      <- noteT "No sequence assigned"
                  (hoistMaybe $ tuCurrentStimulus loggedInUser)
  ssi          <- noteT "Bad seq lookup" $ MaybeT $ gh $ get itemKey
  noteT "Bad resource lookup" $ MaybeT $ gh $ get (ssiStimulus ssi)

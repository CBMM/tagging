{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}

module Server.User where

import Control.Error
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Logger (NoLoggingT)
import Database.Groundhog
import Database.Groundhog.Postgresql (Postgresql)
import GHC.Int
import Snap.Core
import Snap.Snaplet
import qualified Data.Aeson as A
import Snap.Snaplet.Groundhog.Postgresql

import Tagging.Stimulus
import Tagging.Response
import Tagging.User
import Server.Application
import Server.Crud
import Server.Resources
import Server.Utils

------------------------------------------------------------------------------
-- | Request the next stimulus in the user's assigned sequence
--   @Nothing@ return values indicates the sequence is finished,
--   Other sorts of errors are signaled with normal http response codes
handleRequestCurrentStimulus :: Handler App App ()
handleRequestCurrentStimulus = eitherT err300 json $ do

  TaggingUser{..} <- noteT "TaggingUser retrieval error" getCurrentTaggingUser
  StimSeqItem{..} <- noteT "Problem" $ (MaybeT . gh . get)
                     =<< hoistMaybe tuCurrentStimulus
  noteT "Bad stimulus lookup" $ MaybeT $ gh $ get ssiStimulus


------------------------------------------------------------------------------
-- | Submit a response. Submission will update the user's current-stimulus
--   field to @Just@ `the next sequence stimulus` if there is one, or to
--   @Nothing@ if the sequence is done
handleSubmitResponse :: Handler App App ()
handleSubmitResponse = eitherT err300 (const $ return ()) $ do

  loggedInUser           <- noteT "No logged in tagging user"
                            getCurrentTaggingUser
  r@StimulusResponse{..} <- (hoistEither . A.eitherDecode)
                            =<< (lift $ readRequestBody 1000000)
  stim                   <- noteT "Bad stim lookup from response"
                            $ MaybeT $ gh $ get srStim
  respUser               <- crudGet srUser

  when (tuId loggedInUser /= tuId respUser)
    (lift $ err300 "Logged in user / reported user mismatch")

  lift . gh $ do
    insert r
    insert (loggedInUser {tuCurrentStimulus = ssiNextItem stim})

getCurrentStimulusResource :: Handler App App ()
getCurrentStimulusResource = eitherT err300 json $ do
  loggedInUser <- noteT "No logged it tagging user" getCurrentTaggingUser
  itemKey      <- noteT "No sequence assigned"
                  (hoistMaybe $ tuCurrentStimulus loggedInUser)
  ssi          <- noteT "Bad seq lookup" $ MaybeT $ gh $ get itemKey
  noteT "Bad resource lookup" $ MaybeT $ gh $ get (ssiStimulus ssi)

-- ------------------------------------------------------------------------------
-- getNextStimulus :: Maybe (AutoKey StimSeqItem)
--                 -> DbPersist Postgresql (NoLoggingT IO)
--                    (Maybe (AutoKey StimSeqItem))
-- getNextStimulus Nothing  = return Nothing
-- getNextStimulus (Just k) = runMaybeT $ do
--   StimSeqItem{..} <- MaybeT $ get k
--   --lift $ insert (undefined :: StimSeqItem)
--   --lift $ return (undefined :: Maybe (AutoKey StimSeqItem))
--   k' <- lift $ 
--         select $ (SsiIndexField >. ssiIndex &&. SsiStimSeqField ==. ssiStimSeq)
--                  `orderBy` [Asc (SsiStimulusField)]
--   --lift (selectAll :: DbPersist Postgresql (NoLoggingT IO) [(AutoKey StimSeqItem, StimSeqItem)])
--   MaybeT $ return (listToMaybe k')

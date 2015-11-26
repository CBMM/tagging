{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Server.Subject where

import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (except)
import           Control.Monad.Logger       (NoLoggingT)
import qualified Data.ByteString.Char8      as B8
import qualified Data.List as L
import           Data.Proxy
import qualified Data.Text as T
import           Data.Time
import           Database.Groundhog
import           Database.Groundhog.Postgresql
import           Database.Groundhog.Postgresql.Array
import           GHC.Generics
import           GHC.Int
import           Servant
import           Servant.Docs
import           Servant.Server
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple
import qualified Data.Aeson as A

import           Tagging.Stimulus
import           Tagging.Response
import           Tagging.User
import           Server.Application
import           Server.Crud
import           Server.Database
import           Server.Resources
import           Server.Utils


------------------------------------------------------------------------------
type SubjectAPI = "currentstim"     :> Get '[JSON] StimSeqItem
             :<|> "currentsequence" :> Get '[JSON] StimulusSequence
             :<|> "posinfo"         :> Get '[JSON] PositionInfo
             :<|> "fullposinfo"     :> Get '[JSON] (Maybe
                                       (PositionInfo,
                                        StimulusSequence,
                                        StimSeqItem))
             :<|> "response"        :> QueryFlag "advance" :> ReqBody '[JSON] ResponsePayload
                                    :> Post '[JSON] ()

------------------------------------------------------------------------------
subjectServer :: Server SubjectAPI AppHandler
subjectServer = handleCurrentStimSeqItem
           :<|> handleCurrentStimulusSequence
           :<|> handleCurrentPositionInfo
           :<|> handleFullPosInfo
           :<|> handleSubmitResponse

-- ------------------------------------------------------------------------------
-- -- | Add or revoke roles on a user
-- assignRoleTo :: AutoKey TaggingUser -> Role -> Bool -> Handler App App ()
-- assignRoleTo targetKey r b = exceptT Server.Utils.err300 (\_ -> return ()) $ do
--   lift $ assertRole [Admin]
--   tu <- noteT "Bad user lookup" $ MaybeT $ runGH $ get targetKey
--   let roles' = (if b then L.union [r] else L.delete r) $ tuRoles tu
--   lift $ runGH $ replace targetKey (tu {tuRoles = roles'})

-- handleAssignRoleTo :: Handler App App ()
-- handleAssignRoleTo = void $ runMaybeT $ do
--   userKey   <- return undefined -- TODO
--   theRole   <- MaybeT (getParam "role")
--   role      <- hoistMaybe (readMay $ B8.unpack theRole)
--   theUpDown <- MaybeT (getParam "bool")
--   upDown    <- hoistMaybe (readMay $ B8.unpack theUpDown)
--   lift $ assignRoleTo userKey role upDown


------------------------------------------------------------------------------
-- | Submit a response. Submission will update the user's current-stimulus
--   field to @Just@ `the next sequence stimulus` if there is one, or to
--   @Nothing@ if the sequence is done
--handleSubmitResponse :: StimulusResponse -> Handler App App ()
handleSubmitResponse :: Bool -> ResponsePayload -> Handler App App ()
handleSubmitResponse advanceStim t =
  exceptT Server.Utils.err300 (const $ return ()) $ do

    u        <- getCurrentTaggingUser
    pos      <- noteT "No assigned stimulus" $ hoistMaybe (tuCurrentStimulus u)
    let i = _piStimSeqIndex pos

    thisReq  <- noteT "No request record by user for stimulus"
                $ MaybeT $ fmap listToMaybe $ runGH
                $ select $ (SreqUserField ==. tuId u
                           &&. SreqStimSeqItemField ==. pos)
                           `orderBy` [Asc SreqTimeField]
    tNow     <- lift $ liftIO getCurrentTime

    stim     <- noteT "Bad stim lookup from response" $ MaybeT $ runGH
                $ get (_piStimulusSequence pos)
                -- $ get (intToKey (Proxy :: Proxy StimulusSequence)
                --                 (_piStimulusSequence pos))

    -- TODO: How to query the array length in groundhog?
    -- TODO This is definitely a (runtime) name error
    l <- lift $ runGH $ count (SsiStimulusSequenceField ==. _piStimulusSequence pos)
    -- [Only l] <- lift $ with db $
    --        query
    --        "SELECT array_length(\"StimSeqItems\") FROM \"StimulusSequence\" WHERE id = ?"
    --        (Only (_piStimulusSequence pos))
    when advanceStim $ lift . runGH $ do
      insert (StimulusResponse (tuId u) pos
              (sreqTime thisReq) tNow "sometype" (rpJson t))
      let p' | i == l - 1 = Nothing
             | otherwise    = Just $ pos & over piStimSeqIndex succ
      update [TuCurrentStimulusField =. p'] (TuIdField ==. tuId u)


------------------------------------------------------------------------------
getCurrentPositionInfo :: AppHandler (Maybe PositionInfo)
getCurrentPositionInfo = exceptT Server.Utils.err300 return $ do
  u   <- getCurrentTaggingUser
  return (tuCurrentStimulus u)

handleCurrentPositionInfo :: AppHandler PositionInfo
handleCurrentPositionInfo =
  maybeT (Server.Utils.err300 "No stmilusus sequence assigned") return
  $ MaybeT getCurrentPositionInfo

getCurrentStimSeqItem :: AppHandler (Maybe StimSeqItem)
getCurrentStimSeqItem = do
  res <- getCurrentPositionInfo
  case res of
    Nothing -> error "NoPosInfo" -- TODO
    Just pInfo@(PositionInfo key i) -> do
      u :: TaggingUser <- exceptT (const $ error "Bad lookup") return getCurrentTaggingUser
      t <- liftIO getCurrentTime
      modifyResponse $ Snap.Core.addHeader "Cache-Control" "no-cache"
      ssi <- runGH $ select $ (SsiStimulusSequenceField ==. key &&. SsiIndexField ==. i)
      case ssi of
        [] -> return Nothing
        [ssi] -> do
          runGH $ insert (StimulusRequest (tuId u) pInfo t)
          return $ Just ssi


handleCurrentStimSeqItem :: AppHandler StimSeqItem
handleCurrentStimSeqItem =
  maybeT (Server.Utils.err300 "No stmilusus sequence assigned") return
  $ MaybeT getCurrentStimSeqItem

getCurrentStimulusSequence :: AppHandler (Maybe StimulusSequence)
getCurrentStimulusSequence = do
  res <- getCurrentPositionInfo
  case res of
    Nothing -> error "NoUser" -- TODO
    Just pInfo@(PositionInfo key _) -> do
      ssi <- runGH $ get key
      return ssi

handleCurrentStimulusSequence :: AppHandler StimulusSequence
handleCurrentStimulusSequence =
  maybeT (error "Bad sequence lookup") return 
  (MaybeT getCurrentStimulusSequence)

handleFullPosInfo 
  :: AppHandler (Maybe (PositionInfo, StimulusSequence, StimSeqItem))
handleFullPosInfo = 
  maybeT (return Nothing) (return . Just) $ (,,) <$> MaybeT getCurrentPositionInfo
                                                 <*> MaybeT getCurrentStimulusSequence
                                                 <*> MaybeT getCurrentStimSeqItem

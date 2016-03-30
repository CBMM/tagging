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
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Time
import           Database.Groundhog
import           Database.Groundhog.Postgresql
import           Database.Groundhog.Postgresql.Array
import           GHC.Generics
import           GHC.Int
import           System.Random
------------------------------------------------------------------------------
import           Servant
import           Servant.Docs
import           Servant.Server
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple
import qualified Data.Aeson as A
------------------------------------------------------------------------------
import           Tagging.Stimulus
import           Tagging.Response
import           Tagging.User
import           Server.Application
import           Server.Crud
import           Server.Database
import           Server.Resources
import           Server.Utils
import qualified Utils as Utils


------------------------------------------------------------------------------
type SubjectAPI = "currentstim"       :> Get '[JSON] StimSeqItem
             :<|> "currentsequence"   :> Get '[JSON] StimulusSequence
             :<|> "currentassignment" :> Get '[JSON] Assignment
             :<|> "fullposinfo"       :> Get '[JSON] (Maybe
                                         (Assignment,
                                          StimulusSequence,
                                          StimSeqItem))
             :<|> "progress"          :> Get '[JSON] Progress
             :<|> "response"          :> QueryFlag "advance" :> ReqBody '[JSON] ResponsePayload
                                      :> Post '[JSON] ()

------------------------------------------------------------------------------
subjectServer :: Server SubjectAPI AppHandler
subjectServer = handleCurrentStimSeqItem
           :<|> handleCurrentStimulusSequence
           :<|> handleCurrentAssignment
           :<|> handleFullPosInfo
           :<|> handleProgress
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
--   field to @Just@ `the next sequence stimulus` if there is one, or tno
--   @Nothing@ if the sequence is done
--handleSubmitResponse :: StimulusResponse -> Handler App App ()
handleSubmitResponse :: Bool -> ResponsePayload -> Handler App App ()
handleSubmitResponse advanceStim t =
  exceptT Server.Utils.err300 (const $ return ()) $ do

    u                 <- getCurrentTaggingUser
    asgn <- noteT "No assignment" $ MaybeT getCurrentAssignment
    let (Assignment aU s i) = asgn
    let s'' = s :: DefaultKey StimulusSequence
    let i' = fromIntegral (i :: Int) :: Int64

    thisReq  <- noteT "No request record by user for stimulus"
                $ MaybeT $ fmap listToMaybe $ runGH
                $ select $ (SreqUserField ==. tuId u
                           &&. SreqSequenceField ==. s
                           &&. SreqIndexField    ==. i')
                           `orderBy` [Asc SreqTimeField]
    thisSeq  <- noteT "No such stimulus sequence" $ MaybeT $ runGH $ get s
    tNow     <- lift $ liftIO getCurrentTime

    stim     <- noteT "Bad stim lookup from response" $ MaybeT $ runGH
                $ get s

    l <- lift $ runGH $ count (SsiStimulusSequenceField ==. s)
    lift . runGH $ do
      insert (StimulusResponse (tuId u) -- TODO drop old posinfos
                                 s
                                 (fromIntegral i)
                (sreqTime thisReq) tNow "sometype" (rpJson t))

      when advanceStim $ case ssSampling thisSeq of
        SampleIncrement -> do
          let x = tuId u :: Int64
          if i == l - 1
            then do
              let u'' = tuId u :: Int64
              deleteBy (Utils.integralToKey (tuId u) :: DefaultKey Assignment)
              return ()
            else do
              let k'  = Utils.integralToKey (tuId u) :: DefaultKey TaggingUser
                  k'' = Utils.integralToKey (tuId u) :: DefaultKey Assignment
                  a'  = Assignment k' s (succ i)
              update [AIndexField =. succ i]
                (AUserField ==. k'
                 &&. ASequenceField ==. s)
        SampleRandomNoReplacement -> do
          resps <- fmap (S.fromList . fmap srIndex) $
                   select (SrSequenceField ==. s'' &&.
                           SrUserField ==. tuId u)
          allInds <- (\n -> S.fromList $ take n [0..]) <$> count (SsiStimulusSequenceField ==. s'')
          let remainingInds = S.difference allInds resps
          if S.null remainingInds
          then deleteBy (Utils.integralToKey (tuId u) :: DefaultKey Assignment)
          else do
            i <- liftIO $ randomRIO (0,S.size remainingInds)
            let newAssignmentInd = S.toList remainingInds !! i
            update [AIndexField =. (fromIntegral newAssignmentInd :: Int)]
                   (AUserField ==. (Utils.integralToKey (tuId u) :: DefaultKey TaggingUser)
                    &&. ASequenceField ==. s)


------------------------------------------------------------------------------
getCurrentAssignment :: AppHandler (Maybe Assignment)
getCurrentAssignment = runMaybeT $ do
  u   <- hushT getCurrentTaggingUser
  let uKey = Utils.integralToKey (tuId u) :: DefaultKey TaggingUser
  MaybeT $ fmap listToMaybe $ runGH $ select (uKey ==. AUserField)


handleCurrentAssignment :: AppHandler Assignment
handleCurrentAssignment =
  maybeT (Server.Utils.err300 "No stmilusus sequence assigned") return
  $ MaybeT getCurrentAssignment


handleProgress :: AppHandler Progress
handleProgress = do
  asgn <- getCurrentAssignment
  case asgn of
    Nothing -> Server.Utils.err300 "No assignment"
    Just (Assignment u sID sIndex) -> do
      nSequenceStims <- runGH $ count (SsiStimulusSequenceField ==. sID)
      userResps     <- runGH $ count (SrUserField ==. (keyToInt u) &&.
                              SrSequenceField ==. sID)
      return $ Progress userResps nSequenceStims


getCurrentStimSeqItem :: AppHandler (Maybe StimSeqItem)
getCurrentStimSeqItem = do
  res <- getCurrentAssignment
  case res of
    Nothing -> error "No PosInfo" -- TODO
    Just (Assignment _ key i) -> do
      u <- exceptT (const $ error "Bad lookup") return getCurrentTaggingUser
      t <- liftIO getCurrentTime
      modifyResponse $ Snap.Core.addHeader "Cache-Control" "no-cache"
      ssi <- runGH $ select (SsiStimulusSequenceField ==. key &&.
                             SsiIndexField ==. i)
      case ssi of
        [] -> return Nothing
        [ssi] -> do
          runGH $ insert (StimulusRequest (tuId u)
                                          (key)
                                          (fromIntegral i)
                                          t)
          return $ Just ssi


handleCurrentStimSeqItem :: AppHandler StimSeqItem
handleCurrentStimSeqItem =
  maybeT (Server.Utils.err300 "No stmilusus sequence assigned") return
  $ MaybeT getCurrentStimSeqItem

getCurrentStimulusSequence :: AppHandler (Maybe StimulusSequence)
getCurrentStimulusSequence = do
  res    <- getCurrentAssignment
  case res of
    Nothing -> error "NoUser" -- TODO
    Just (Assignment _ key i) -> do
      ssi <- runGH $ get key
      return ssi

handleCurrentStimulusSequence :: AppHandler StimulusSequence
handleCurrentStimulusSequence =
  maybeT (error "Bad sequence lookup") return
  (MaybeT getCurrentStimulusSequence)

handleFullPosInfo
  :: AppHandler (Maybe (Assignment, StimulusSequence, StimSeqItem))
handleFullPosInfo =
  maybeT (return Nothing) (return . Just) $ (,,) <$> MaybeT getCurrentAssignment
                                                 <*> MaybeT getCurrentStimulusSequence
                                                 <*> MaybeT getCurrentStimSeqItem

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
import qualified Data.List                  as L
import           Data.Monoid
import           Data.Proxy
import qualified Data.Set                   as S
import           Data.String                (fromString)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Time
import           Database.Groundhog
import qualified Database.Groundhog.Core    as G
import qualified Database.Groundhog.Generic as G
import           Database.Groundhog.Postgresql
import           Database.Groundhog.Postgresql.Array
import           GHC.Generics
import           GHC.Int
import qualified Network.Http.Client        as C
import           System.Random
import qualified System.IO.Streams          as S
import qualified URI.ByteString             as URI
------------------------------------------------------------------------------
import           Servant
import           Servant.Docs
import           Servant.Server
import           Snap.Core
import           Snap.Snaplet
-- import           Snap.Snaplet.PostgresqlSimple
import qualified Data.Aeson as A
------------------------------------------------------------------------------
import           Tagging.API
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
subjectServer :: Server SubjectAPI AppHandler
subjectServer = handleCurrentStimSeqItem
           :<|> handleCurrentStimulusSequence
           :<|> handleCurrentAssignment
           :<|> handleFullPosInfo
           :<|> handleProgress
           :<|> handleSubmitResponse
           :<|> handleAnswerKey

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
handleSubmitResponse :: Bool -> ResponsePayload -> Handler App App ()
handleSubmitResponse advanceStim t = do

  prog@(Progress nAnswers nQuestions) <- handleProgress
  asgn@(Assignment aU s maybeI rngS rngE finishURL) <- handleCurrentAssignment

  exceptT Server.Utils.err300 (const $ return ()) $ do

    tNow     <- lift $ liftIO getCurrentTime
    u                 <- getCurrentTaggingUser
    i <- case maybeI of
      Nothing -> error "Unexpected no-assignment when submitting response" -- TODO proper error handling
      Just i  -> return i
    let s'' = s :: DefaultKey StimulusSequence
    let i' = fromIntegral (i :: Int) :: Int64
    checkStimulusBounds i (rngS, rngE)

    thisReq  <- case i of
      -- Special-case for index '-1': There is no stim-seq-item
      (-1) -> return $ StimulusRequest (tuId u) s i' tNow
      _    -> noteT "No request record by user for stimulus"
                $ MaybeT $ fmap listToMaybe $ runGH
                $ select $ (SreqUserField ==. tuId u
                           &&. SreqSequenceField ==. s
                           &&. SreqIndexField    ==. i')
                           `orderBy` [Asc SreqTimeField]
    thisSeq  <- noteT "No such stimulus sequence" $ MaybeT $ runGH $ get s

    stim     <- noteT "Bad stim lookup from response" $ MaybeT $ runGH
                $ get s

    l <- lift $ runGH $ count (SsiStimulusSequenceField ==. s)
    lift . runGH $ do
      insert (StimulusResponse (tuId u) -- TODO drop old posinfos
                                 s
                                 (fromIntegral i)
                (sreqTime thisReq) tNow "sometype" (rpJson t))

      when advanceStim $ case ssSampling thisSeq of
        SampleIndex -> error "No advance allowed for SampleIndex SamplingMethod experiments"
        SampleIncrement -> do
          let index' = maybe Nothing (\i -> bool (Just $ succ i) Nothing (i >= rngE)) maybeI
              asgn'  = asgn { aIndex = index' }
          let u'' = tuId u :: Int64
          let x = tuId u :: Int64
              k'  = Utils.integralToKey (tuId u) :: DefaultKey TaggingUser
              answeredLastStimulus = i == rngE

          when answeredLastStimulus $
              maybe (return ()) (liftIO . runFinishedURL) finishURL

          update [AIndexField =. index'] -- TODO
                (AUserField ==. k'
                 &&. ASequenceField ==. s)

        SampleRandomNoReplacement -> do

          let u'' = tuId u :: Int64
              pxy = Proxy  :: Proxy Postgresql

          let cse = "WITH answered as (SELECT ssi_index FROM stimulus_response "
                                   ++ "INNER JOIN stim_seq_item "
                                   ++ "ON ssi_index = sr_index AND ssi_stimulus_sequence = sr_sequence "
                                   ++ "WHERE sr_sequence = ? AND sr_user = ?) "
          [nAnswered] :: [Int] <- queryRaw False
                          (cse ++ "SELECT count(*) FROM answered")
                          [ G.toPrimitivePersistValue pxy s''
                          , G.toPrimitivePersistValue pxy u''] $
                          G.mapAllRows (fmap fst . G.fromPersistValues)

          nRemaining <- fmap (+ negate nAnswered) $ count (SsiStimulusSequenceField ==. s'')

          nextStim :: [Int] <- queryRaw False
                      (cse
                       ++ "(SELECT ssi_index FROM stim_seq_item "
                       ++ "WHERE ssi_stimulus_sequence = ?) "
                       ++ "EXCEPT (SELECT ssi_index FROM answered) "
                       ++ "OFFSET floor(random() * " ++ show nRemaining ++ ") LIMIT 1"
                      ) [ G.toPrimitivePersistValue pxy s''
                        , G.toPrimitivePersistValue pxy u''
                        , G.toPrimitivePersistValue pxy s''] $
                      G.mapAllRows (fmap fst . G.fromPersistValues)

          liftIO $ print $ "NEXT STIM: " ++ show nextStim
          case nextStim of

            -- TODO: This is a case of the one asgn per (user,experiment) assumption
            [] -> do update [AIndexField =. (Nothing :: Maybe Int)]
                            (AUserField ==. (Utils.integralToKey (tuId u) :: DefaultKey TaggingUser)
                            &&. ASequenceField ==. s)
                     maybe (return ()) (liftIO . runFinishedURL) finishURL
            -- TODO Document this. Used to delete the Assignment row. Now ignoring
            -- []    -> deleteBy (Utils.integralToKey (tuId u) :: DefaultKey Assignment)
            (n:_) -> do
              -- i <- liftIO $ randomRIO (0,S.size remainingInds)
              -- let newAssignmentInd = S.toList remainingInds !! i
              update [AIndexField =. Just (fromIntegral n :: Int)]
                     (AUserField ==. (Utils.integralToKey (tuId u) :: DefaultKey TaggingUser)
                      &&. ASequenceField ==. s)

runFinishedURL :: T.Text -> IO ()
runFinishedURL url = do

  let (Right pieces) = URI.parseURI URI.strictURIParserOptions (T.encodeUtf8 url)
      pairs          = URI.queryPairs (URI.uriQuery pieces)
  print $ "POSTing to: " <> T.unpack url -- TODO this is not an error. How to log non-errors?
  r <- C.postForm (T.encodeUtf8 url) pairs (\_ b -> S.read b) -- TOOD this assumes b is small (one stream chunk)
  print "RESPONSE:"
  print r


-- ------------------------------------------------------------------------------
-- -- | Check whether the logged-in user's assignment is finished
-- experimentIsOver :: AppHandler (Maybe Bool)
-- experimentIsOver = do
--   r <- handleFullPosInfo Nothing
--   case r of
--     Nothing -> error "No assignment" -- TODO proper error handling
--     Just ((Assignment _ _ i _ e _),
--           (StimulusSequence _ _ _ _ _ sampling),
--           _) -> case sampling of
--       SampleIncrement -> return $ fmap (> e) i
--       SampleRandomNoReplacement -> do
--         (Progress n nTotal) <- handleProgress
--         return (n >= nTotal)



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


handleProgress :: Handler App App Progress
handleProgress = do
  posInfo <- handleFullPosInfo Nothing
  case posInfo of
    Nothing -> Server.Utils.err300 "No assignment"
    Just (FPI (Assignment u sID sIndex rangeStart rangeEnd _) sSeq _) ->
      case ssSampling sSeq of
        SampleIncrement -> do
          let nSequenceStims = rangeEnd - rangeStart + 1
              userResps = maybe nSequenceStims (`subtract` rangeStart) sIndex
            in return $ Progress userResps nSequenceStims
        SampleRandomNoReplacement -> runGH $ do
          let nSequenceStims = rangeEnd - rangeStart + 1
          userResps <- count (SsiStimulusSequenceField ==. sID)
          return (Progress userResps nSequenceStims)


checkStimulusBounds :: MonadSnap m => Int -> (Int, Int) -> m ()
checkStimulusBounds i (rangeStart,rangeEnd) =
  when (i < rangeStart || i > rangeEnd) $
  logError $ "handleSubmitResponse called with index " <> B8.pack (show i)
                 <> " and range (" <> B8.pack (show rangeStart)
                 <> ", " <> B8.pack (show rangeEnd) <> ")"


getCurrentStimSeqItem :: AppHandler (Maybe StimSeqItem)
getCurrentStimSeqItem = do
  res <- getCurrentAssignment
  case res of
    Nothing -> error "No PosInfo" -- TODO
    Just (Assignment _ key Nothing _ _ _ ) -> return Nothing
    Just (Assignment _ key (Just i) rangeStart rangeEnd _) -> do
      checkStimulusBounds i (rangeStart,rangeEnd)
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
    Just (Assignment _ key _ _ _ _) -> do
      ssi <- runGH $ get key
      return ssi

handleCurrentStimulusSequence :: AppHandler StimulusSequence
handleCurrentStimulusSequence =
  maybeT (error "Bad sequence lookup") return
  (MaybeT getCurrentStimulusSequence)

handleFullPosInfo
  :: Maybe Int -> AppHandler (Maybe FullPosInfo) -- (Maybe (Assignment, StimulusSequence, StimSeqItem))
handleFullPosInfo indexRequest = do
  pInfo <- maybeT (return Nothing) (return . Just) $ FPI
    <$> MaybeT getCurrentAssignment
    <*> MaybeT getCurrentStimulusSequence
    <*> lift getCurrentStimSeqItem
  case pInfo of
    Nothing -> error "Bad decoding of fullposinfo" -- TODO
    Just (FPI asgn ss ssi) -> case indexRequest of
      Nothing   -> case ssSampling ss of
        SampleIndex -> error "SamplingMethod is SampleIndex - client must set 'indexRequest' query parameter"
        _           -> return pInfo
      Just iReq -> do
        asgn' <- case ssSampling ss of
          SampleIndex              -> userChooseIndex iReq asgn
          SampleIndexNoReplacement -> error "Not Implemented" -- TODO implement
          _                        -> error "Tried to request a stimulus index when that's not allowed"
        return $ Just $ FPI asgn' ss ssi

  where userChooseIndex :: Int -> Assignment -> AppHandler Assignment
        userChooseIndex i asgn@(Assignment aUsr aSeq aInd rangeStart rangeEnd finishURL) = do
          checkStimulusBounds i (rangeStart, rangeEnd)
          runGH $ update [AIndexField =. Just i] (AUserField ==. aUsr &&. ASequenceField ==. aSeq)
          return (Assignment aUsr aSeq (Just i) rangeStart rangeEnd finishURL)

-------------------------------------------------------------------------------
handleAnswerKey :: Maybe Int -> AppHandler [StimSeqAnswer]
handleAnswerKey Nothing = error "Must pass 'experiment' parameter"
--handleAnswerKey (Just k) qs = do
handleAnswerKey (Just k) = do
  tu :: TaggingUser <- exceptT (Server.Utils.err300) return getCurrentTaggingUser
  let queryProxy  = Proxy :: Proxy Postgresql
      seqKey      = G.toPrimitivePersistValue queryProxy k
      userKey     = G.toPrimitivePersistValue queryProxy (tuId tu)
      -- TODO: We only take the answer key from the last 20 responses,
      --       due to a strange problem accepting a list of preferred indices into the
      --       answer key from the request body, because request body parsing was failing
      --       here (not sure why)
      -- reqRestrict = map G.toPrimitivePersistValue queryProxy $ fromMaybe [] qs
      -- restr       = G.PersistCustom (G.Utf8 . fromString $ "(" <> L.intercalate "," (map show qs) <> ")") []
  runGH (queryRaw False (unlines
         ["WITH resps AS ("
         ,"  SELECT sr_index FROM stimulus_response"
         ,"  WHERE sr_sequence = ? AND sr_user = ? ORDER BY sr_responded_time DESC LIMIT 20)"
         ,"SELECT * FROM stim_seq_answer"
         ," WHERE ssa_stimulus_sequence = ? "
         ," AND ssa_index IN (SELECT sr_index FROM resps"
         ,"                   INTERSECT "
         ,"                   (SELECT ssa_index FROM stim_seq_answer as sr_index) ) "
         -- ," AND ssa_index IN ?"
         ])
         [seqKey, userKey, seqKey]
--          [seqKey, userKey, seqKey,restr]
        (G.mapAllRows (fmap fst . G.fromPersistValues)))

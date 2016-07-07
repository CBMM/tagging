{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Server.Researcher where

-------------------------------------------------------------------------------
import Control.Error
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B8
import Data.Proxy
import Data.Monoid ((<>))
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Expression
import Database.Groundhog.TH
import GHC.Int
import Servant.API
import qualified Servant.API.ResponseHeaders as H
import qualified Network.HTTP.Types as H
import Servant.Server
import Snap.Core (logError, liftSnap, writeText)
import Snap.Snaplet
import Snap.Snaplet.Auth (AuthUser, UserId(..), currentUser,userId, unUid)
import Text.Read hiding (get)
-------------------------------------------------------------------------------
import Tagging.User
import Tagging.Stimulus
import Tagging.Response
-------------------------------------------------------------------------------
import Server.Database
import Server.Resources
import qualified Server.GroundhogAuth as G
import Server.Utils hiding (intToKey)
import Server.Application
import Tagging.API
import Utils


-------------------------------------------------------------------------------
-- | API definition used by researchers for admining experiments
researcherServer :: ServerT ResearcherAPI AppHandler
researcherServer = assignUserSeqStart :<|> loadSequence :<|> subjectDataId
              :<|> subjectDataLogin   :<|> getSequence


------------------------------------------------------------------------------
{-| Assign a user to the first index of a @StimulusSequence@.
    If the logged-in user is Admin or Researcher, they may
    assign any user to any sequence.
    Otherwise, the assignment will only work if the logged-in user's
    id matches the user id being assigned, and the @StimulusSequence@
    is marked self-assignable in the SubjectCanSelfAssign table.
|-}
assignUserSeqStart :: Maybe Int64 -- ^ User to assign
                   -> Maybe Int64 -- ^ Stim Seq to assign
                   -> Maybe Int64 -- ^ Optional assignment range start
                   -> Maybe Int64 -- ^ Optional assignment range end
                   -> Maybe T.Text  -- ^ Completion URL
                   -> Handler App App ()
assignUserSeqStart (Just userIdParam) (Just seqId) seqStart seqEnd finishedUrl = do
  let uKey   = intToKey (fromIntegral userIdParam) :: DefaultKey TaggingUser
      seqKey = intToKey (fromIntegral seqId)  :: DefaultKey StimulusSequence

  needsPrivileges <- do
    noSelfAssign  <- runGH $ (== 0) <$> count (ScsaSequenceField ==. seqKey)
    let targetParamId = Just $ UserId $ T.pack $ show userIdParam
    wrongLogin    <- with auth $ (\mu -> targetParamId /= (userId =<< mu))
                                 <$> currentUser
    return $ noSelfAssign || wrongLogin
  when needsPrivileges $ assertRole [Admin,Researcher]

  result <- runGH $ do
    uQury <- get uKey
    case uQury of
      Nothing -> return $
        Left ("Found no user with id " ++ show userIdParam)
      Just _  -> do
        nAsgn <- count (AUserField                 ==. uKey
                        &&. ASequenceField         ==. seqKey)
        seqInds <- map ssiIndex <$>
          select (SsiStimulusSequenceField ==. seqKey)
        let (indMin, indMax) = (minimum seqInds, maximum seqInds)
        let asgnStart = fromMaybe indMin (fmap fromIntegral seqStart)
            asgnEnd   = fromMaybe indMax (fmap fromIntegral seqEnd)
        case nAsgn of
          0 -> insert (Assignment uKey seqKey (Just asgnStart)
                                  asgnStart asgnEnd finishedUrl)
               >> return (Right ())
          n -> do
              update [AIndexField =. Just asgnStart
                     ,AStartField =. asgnStart
                     , AEndField  =. asgnEnd ]
                     (AUserField ==. uKey &&. ASequenceField ==. seqKey)
              return $ bool
                (Left ("assignUserSeqStart encountered user/experiment combo "
                      ++ "with more than 1 entry. Updating them all"))
                (Right ())
                (n == 1)
  case result of
    Left e   -> Server.Utils.err300 e
    Right () -> return ()


------------------------------------------------------------------------------
{-| Insert a pair of @StimulusSequence@ and @[StimSeqItem]@ into their
    respective tables |-}
loadSequence :: (StimulusSequence, [StimSeqItem]) -> AppHandler Int64
loadSequence (stimSeq, ssItems) = do
  assertRole [Admin, Researcher]
  runGH $ do
    seqKey <- insert stimSeq
    forM_ ssItems $ \ssItem -> insert ssItem {ssiStimulusSequence = seqKey}
    return (fromIntegral $ Utils.keyToInt seqKey)


------------------------------------------------------------------------------
{-| Retrieve a @StimulusSequence@ and all its @StimSeqItem@s |-}
getSequence :: Int64 -> AppHandler (StimulusSequence, [StimSeqItem])
getSequence seqID = do
  assertRole [Admin, Researcher]
  let ssKey = integralToKey seqID :: DefaultKey StimulusSequence
  ss :: Maybe StimulusSequence <- runGH $ get ssKey
  case ss of
    Nothing   -> Server.Utils.err300 "Unknown stimusul sequence"
    Just sseq -> do
      items <- runGH $
        select $ (SsiStimulusSequenceField ==. ssKey)
                 `orderBy` [Asc SsiIndexField]
      return (sseq, items)

------------------------------------------------------------------------------
{-| Download all @StimulusResponse@ entries from a given user on a given
    experiment
|-}
subjectDataId
  :: Int64 -- ^ User ID
  -> Int64 -- ^ StimulusSequence Id
  -> AppHandler (Headers '[Header "Content-Disposition" String]
                 [StimulusResponse])
subjectDataId userId seqId = do
  assertRole [Admin, Researcher]
  let sKey = integralToKey seqId :: DefaultKey StimulusSequence
      q    = (SrUserField ==. userId &&. SrSequenceField ==. sKey)
      hdr  = B8.concat [ "attachment; filename=\"taggingdata-"
                       , B8.pack (show (userId :: Int64)) , "-"
                       , B8.pack (show (seqId :: Int64)) , ".json\""
                       ]
  res <- runGH $ select $ q `orderBy` [Asc SrIndexField]
  return $ H.addHeader (B8.unpack hdr) res


subjectDataLogin
  :: Maybe T.Text -- ^ User Login
  -> Maybe Int64  -- ^ StimulusSequence Id
  -> AppHandler (Headers '[Header "Content-Disposition" String]
                 [StimulusResponse])
subjectDataLogin (Just userLogin) (Just seqId) = do
  let sKey = integralToKey seqId :: DefaultKey StimulusSequence
  userIds <- runGH $ select (G.UserLoginField ==. userLogin)
  case userIds of
    []  -> Server.Utils.err300 . T.unpack $ "No users with login: " <> userLogin
    [u] -> maybe (error "No userid parse")
                 (\i -> subjectDataId i seqId)
                 (authUserId u)
    _   -> error "Impossible case: Multiple users with the same login"
subejctDataLogin _ _ = error "Pass a userlogin and sequence query param"

authUserId :: AuthUser -> Maybe Int64
authUserId a = join $ ((readMaybe . T.unpack . unUid) <$>) (userId a)

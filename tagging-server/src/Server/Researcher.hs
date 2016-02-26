{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Server.Researcher where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B8
import Data.Proxy
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Expression
import Database.Groundhog.TH
import GHC.Int
import Control.Error
import Control.Monad
import Servant.API
import qualified Servant.API.ResponseHeaders as H
import qualified Network.HTTP.Types as H
import Servant.Server
-- import Snap.Core
import Snap.Snaplet

import Tagging.User
import Tagging.Stimulus
import Tagging.Response

import Server.Database
import Server.Resources
import Server.Utils hiding (intToKey)
import Server.Application
import API
import Utils


researcherServer :: ServerT ResearcherAPI AppHandler
researcherServer = assignUserSeqStart :<|> loadSequence
              :<|> subjectData        :<|> getSequence


------------------------------------------------------------------------------
assignUserSeqStart :: Int64 -> Int64 -> Handler App App ()
assignUserSeqStart userId seqId = do
  let uKey   = intToKey (fromIntegral userId) :: DefaultKey TaggingUser
      seqKey = intToKey (fromIntegral seqId)  :: DefaultKey StimulusSequence
  assertRole [Admin,Researcher]
  result <- runGH $ do
    uQury <- get uKey
    case uQury of
      Nothing -> return $ Left ("Found no user with id " ++ show userId)
      Just _  -> do
        nAsgn <- count (AUserField                 ==. uKey
                        &&. ASequenceField         ==. seqKey)
        seqMin <- select $ (SsiStimulusSequenceField ==. seqKey)
                           `orderBy` [Asc SsiIndexField]
                           `limitTo` 1
        case (seqMin :: [StimSeqItem]) of
          []      -> return $ Left "No stimuli found for sequence"
          [item0] -> case nAsgn of
            0 -> insert (Assignment uKey seqKey (ssiIndex item0)) >>
                 return (Right ())
            _ -> do
              update [AIndexField =. ssiIndex item0]
                     (AUserField ==. uKey &&. ASequenceField ==. seqKey)
              return (Right ())
          _ -> return $ Left "Impossible case: too many matches"
  case result of
    Left e   -> Server.Utils.err300 e
    Right () -> return ()


------------------------------------------------------------------------------
loadSequence :: (StimulusSequence, [StimSeqItem]) -> AppHandler Int64
loadSequence (stimSeq, ssItems) = do
  assertRole [Admin, Researcher]
  runGH $ do
    seqKey <- insert stimSeq
    forM_ ssItems $ \ssItem -> insert ssItem {ssiStimulusSequence = seqKey}
    return (fromIntegral $ Utils.keyToInt seqKey)

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
subjectData :: Int64
            -> Int64
            -> AppHandler
               (Headers '[Header "Content-Disposition" String]
                [StimulusResponse])
subjectData userId seqId = do
  assertRole [Admin, Researcher]
  let sKey = intToKey (fromIntegral seqId)  :: DefaultKey StimulusSequence
      disposition = B8.concat
                    [ "attachment; filename=\"taggingdata-"
                    , B8.pack (show (userId :: Int64))
                    , "-"
                    , B8.pack (show (seqId :: Int64))
                    , ".json\""
                    ]
  res <- runGH $ select (SrUserField         ==. userId
                  &&. SrSequenceField ==. sKey)
  return $ H.addHeader (B8.unpack disposition) res

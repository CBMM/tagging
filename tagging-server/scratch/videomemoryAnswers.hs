{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Aeson
import qualified Data.Aeson as A
import Data.Foldable
import Data.Proxy
import Database.Groundhog
import Database.Groundhog.Postgresql
import GHC.Generics
import System.Environment
import Server.Application
-- import Server.Utils
import Utils
import Tagging.Stimulus

-- Copy-pasted data types from the front-end code
data ClipResponse = ClipResponse
  { _crRemember :: Bool
  } deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON   ClipResponse
instance FromJSON ClipResponse

data Survey = Survey
              deriving (Show,Eq,Generic)
instance A.ToJSON Survey
instance A.FromJSON Survey

data MemoryQuiz = MemoryQuiz
              deriving (Show,Eq,Generic)

instance A.ToJSON MemoryQuiz
instance A.FromJSON MemoryQuiz

data Response = RClip   ClipResponse
              | RSurvey Survey
              | RQuiz   MemoryQuiz
  deriving (Eq, Show, Generic)

instance ToJSON   Response
instance FromJSON Response


main :: IO ()
main = do
  [host,pw] <- getArgs
  run host pw

run :: Host -> Password -> IO ()
run host pw = do
  (stimseq, seqitems) <- getSetup
  withPostgresqlConn (dbString host pw) $ insertAnswers (map (entryToSsa k) seqitems)
  print "Done"

type Password = String
type Host = String


dbString :: Host -> Password -> String
dbString h p = "user=tagging host=" ++ h
  ++ " dbname=tagging" ++ " password=" ++ p
  ++ " port=5432"

f :: String
f = "/home/greghale/Videos/setup3.json"

k :: DefaultKey StimulusSequence
k = integralToKey (5 :: Int)

getSetup :: IO (StimulusSequence, [([FilePath],StimSeqItem)])
getSetup = fmap ((maybe (error "Decode failure") id . A.decode)) $ BSL.readFile f


entryToSsa :: DefaultKey StimulusSequence -- ^ Overwrite the old ssKey, because it wasn't known at the time the setup file was generated
           -> ([FilePath],StimSeqItem)    -- ^ Data used to construct the stim_seq_item (original filenames)
           -> StimSeqAnswer
entryToSsa ssKey (fs, StimSeqItem _ _ ssIndex) = StimSeqAnswer (A.toJSON (RClip (ClipResponse b))) ssKey ssIndex
  where b = case head fs !! 6 of
          '1' -> True
          '4' -> False
          _   -> error "Unexpected filepath"

insertAnswers :: [StimSeqAnswer] -> Postgresql -> IO ()
insertAnswers ssa conn =
  runDbConn (mapM_ insert ssa) conn

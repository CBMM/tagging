module Main where

import System.Environment
import System.IO
import Database.Groundhog.Postgresql
import Tagging.User
import Tagging.Stimulus
import Tagging.Response
import qualified Experiments.SimplePics as SimplePics

main :: IO ()
main = do
  [str] <- getArgs
  withPostgresqlConn str $ runDbConn $ do
    runMigration $ do
      migrate (undefined :: TaggingUser)
      migrate (undefined :: StimulusResource)
      migrate (undefined :: StimulusSequence)
      migrate (undefined :: StimSeqItem)
      migrate (undefined :: StimulusResponse)
    SimplePics.setupStimuli
    return ()

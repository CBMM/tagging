{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import System.IO
import Database.Groundhog.Postgresql
import Tagging.User
import Tagging.Stimulus
import Tagging.Response
import qualified Server.Experiments.SimplePics as SimplePics

main :: IO ()
main = do
  args <- getArgs
  case args of
    [str, admin0pw] -> do
      withPostgresqlConn str $ runDbConn $ do

        runMigration $ do
          migrate (undefined :: TaggingUser)
          migrate (undefined :: StimulusResource)
          migrate (undefined :: StimulusSequence)
          migrate (undefined :: StimSeqItem)
          migrate (undefined :: StimulusResponse)

        --insert admin0

        SimplePics.setupStimuli

      return ()
    _ -> putStrLn $ unwords ["Usage: fixtures"
                            ,"\"user=DBUSER dbname=DATABASE"
                            ,"host=HOST password=DBPASSWORD\""
                            ,"ADMIN_PASSWORD"
                            ,"\n"
                            ,usageExample
                            ]

admin0 :: TaggingUser
admin0 =
  TaggingUser 1 Nothing (Just "Admin")
  Nothing [Admin]

------------------------------------------------------------------------------
usageExample :: String
usageExample =
  unlines
  ["For example, you have set up your postgres database on your local"
  ,"machine with username 'tagging', password 'taggingpassword'. You want"
  ,"the tagging server's administrator password to be 'admin', run the command:"
  ,""
  ,"fixtures \"user=tagging dbname=tagging " ++
   "password=taggingpassword host=localhost\" admin"
  ]

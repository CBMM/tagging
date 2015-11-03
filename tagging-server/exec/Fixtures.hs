{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Configurator
import Data.Monoid
import System.Environment
import System.IO
import Database.Groundhog.Postgresql
import Tagging.User
import Tagging.Stimulus
import Tagging.Response
import qualified Server.Experiments.SimplePics as SimplePics
import qualified Server.Experiments.HomeAlone  as HomeAlone

main :: IO ()
main = do
  args <- getArgs
  case args of
    [configPath] -> do
      cfg <- load [Required configPath]
      hostName <- require cfg "host"
      password <- require cfg "pass"
      dbName   <- require cfg "db"
      userName <- require cfg "user"
      let dbString = unwords ["user="     <> userName
                             ,"dbname="   <> dbName
                             ,"host="     <> hostName
                             ,"password=" <> password
                             ]
      withPostgresqlConn dbString $ runDbConn $ do

        runMigration $ do
          migrate (undefined :: TaggingUser)
          migrate (undefined :: StimulusSequence)
          migrate (undefined :: StimulusRequest)
          migrate (undefined :: StimulusResponse)

        --insert admin0

        --SimplePics.setupStimuli
        --HomeAlone.setupStimuli

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

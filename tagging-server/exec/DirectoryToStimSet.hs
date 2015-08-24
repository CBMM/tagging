{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString as BS
import Data.List ((\\), intercalate)
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Postgresql
import Options.Applicative
import System.Directory
import System.IO

import Tagging.Stimulus

-- data SortBy = Name | Created | Modified
--   deriving (Eq, Show, Read, Enum, Bounded)

-- data DirOpts = DirOpts
--   { directory :: !FilePath
--   , sortBy    :: !SortBy
--   , urlBase   :: !T.Text
--   , title     :: !T.Text
--   , descr     :: !T.Text
--   , host      :: !String
--   , passwd    :: !String
--   } deriving (Show)

-- fullDescription = unwords
--  [ "A program to insert stimulus references into the tagging database.\n"
--  , "Using a local directory as a reference, this tool inserts or updates"
--  , "a StimulusSet entry with metadata provided on the command-line,"
--  , "then inserts one row per file in the directory into each the"
--  , "Stimulusresource and StimulusSequenceItem tables."]

-- dirOpts :: Parser DirOpts
-- dirOpts = DirOpts
--   <$> (option auto (long "path" <> short 'p'
--                    <> help "Path to data directory") <|> pure ".")
--   <*> (option auto (long "sort" <> short 's'
--                    <> help ("Sort by [" ++ sr ++ "]")) <|> pure Name)
--   <*> option auto (long "url" <> short 'u' <> help "Base url")
--   <*> option auto (long "title" <> short 't' <> help "Title")
--   <*> option auto (long "description" <> short 'd' <> help "Description")
--   <*> (strOption (long "host" <> short 'h' <> help "Database host")
--       <|> pure "localhost")
--   <*> strOption   (long "pass" <> help "Database password")
--   where sr = intercalate "|" (map show [minBound..(maxBound :: SortBy)])

-- fullOpts = info (helper <*> dirOpts)
--            (fullDesc
--            <> progDesc fullDescription
--            <> header "directoryToStimSet - a tool for Tagging")

-- main :: IO ()
-- main = do
--   DirOpts{..} <- execParser fullOpts
--   files <- (\\ [".",".."]) <$> getDirectoryContents directory
--   let dbStr = unwords [ "host=" ++ host
--                       , "port=5432"
--                       , "dbname=tagging user=tagging"
--                       , "password=" ++ passwd
--                       ]
--   withPostgresqlConn dbStr $ runDbConn $ do
--     let stimSeq = StimulusSequence title Nothing descr urlBase
--     existingKey <- either id id <$> insertBy SsName stimSeq
--     undefined
main = undefined

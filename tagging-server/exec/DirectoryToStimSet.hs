{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes     #-}

module Main where

import qualified Data.ByteString as BS
import Data.List ((\\), intercalate)
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Postgresql
import Options.Applicative
import Data.String.QQ
import System.Directory
import System.IO

import Tagging.Stimulus

data SortBy = Name | Created | Modified
  deriving (Eq, Show, Read, Enum, Bounded)

vidExtensions :: [FilePath]
vidExtensions = ["mp4","ogg"]

data DirOpts = DirOpts
  { directory :: !FilePath
  , sortBy    :: !SortBy
  , urlBase   :: !T.Text
  , title     :: !T.Text
  , descr     :: !T.Text
  , host      :: !String
  , passwd    :: !String
  } deriving (Show)

fullDescription :: String
fullDescription = [s|
Insert stimulus references into the tagging database.
Using a local directory as a reference, insert a StimulusSequence
entry with metadata provided on the command-line, and links to the
files found in the base directory.

Videos are sorted into subdirectory by extension:
  ${baseDir}/ogg/file.ogg, ${baseDir}/mp4/file.mp4

The file part of the filename (no path, no extension), is concatenated
with an s3 secret key, if one is provided, and the result is hmac-sha256
encoded

|]

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

{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import Data.List ((\\), intercalate)
import qualified Data.Text as T
import Database.Groundhog
import Database.Groundhog.Postgresql
import Options.Applicative
import Data.String.QQ
import System.Directory
import System.FilePath
import System.IO
import Prelude hiding (FilePath)

import Tagging.Stimulus

------------------------------------------------------------------------------
-- | The main point of the utility: grab video files from dir and
--   build a StimulusSequence from them
work :: DirOpts -> IO StimulusSequence
work opt@DirOpts{..} = do

  files <- filterM doesFileExist =<< getDirectoryContents directory

  let fileGroups = groupBy takeBasename . sort $ map takeFilename files

  stimSeqItems <- concat <$> traverse (getFilesByType opt) vidExtensions

  let stimSeq = StimulusSequence title stimSeqItems descr urlBase
  BS.writeFile outFile (A.encode stimSeq)

getFilesByType :: DirOpts -> FilePath -> IO [StimSeqItem]
getFilesByType DirOpts{..} ext = 

data SortBy = Name | Created | Modified
  deriving (Eq, Show, Read, Enum, Bounded)

vidExtensions :: [FilePath]
vidExtensions = ["mp4","ogg"]

data DirOpts = DirOpts
  { directory :: !FilePath
  , sortBy    :: !SortBy
  , urlBase   :: !T.Text
  , outFile   :: !FilePath
  , title     :: !T.Text
  , descr     :: !T.Text
  } deriving (Show)

fullDescription :: String
fullDescription = [s|
Insert stimulus references into the tagging database.
Using a local directory as a reference, insert a StimulusSequence
entry with metadata provided on the command-line, and links to the
files found in the base directory.

Videos are grouped by basename:
  [ [clip1.mov, clip1.ogg], [clip2.mov, clip2.ogg] ... ]

The file part of the filename (no path, no extension), is concatenated
with an s3 secret key, if one is provided, and the result is hmac-sha256
encoded
|]

dirOpts :: Parser DirOpts
dirOpts = DirOpts
  <$> (option auto (long "path" <> short 'p'
                   <> help "Path to data directory") <|> pure ".")
  <*> (option auto (long "sort" <> short 's'
                   <> help ("Sort by [" ++ sr ++ "]")) <|> pure Name)
  <*> option auto (long "url" <> short 'u' <> help "Base url")
  <*> option auto (long "title" <> short 't' <> help "Title")
  <*> option auto (long "description" <> short 'd' <> help "Description")
  <*> (option auto (long "output" <> short 'o' <> help "Output json file")
       <|> pure "./out.json")
  where sr = intercalate "|" (map show [minBound..(maxBound :: SortBy)])

fullOpts = info (helper <*> dirOpts)
           (fullDesc
           <> progDesc fullDescription
           <> header "directoryToStimSet - a tool for Tagging")


main :: IO ()
main = execParser fullOpts >>= work

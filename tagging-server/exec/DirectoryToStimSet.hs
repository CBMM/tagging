{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad                   (filterM)
import qualified Data.Aeson                      as A
import qualified Data.ByteString.Lazy.Char8      as BS
import           Data.Configurator
import           Data.Function                   (on)
import           Data.List                       (groupBy, intercalate, sort)
import qualified Data.Text                       as T
import           Database.Groundhog.Postgresql
import           Database.Groundhog.Postgresql.Array as G
import           Options.Applicative
import           Data.String.QQ
import           System.Directory
import           System.FilePath
import           System.Environment
import           System.IO
import           Prelude                         hiding (FilePath)

import           Tagging.Stimulus
import           Server.Database

------------------------------------------------------------------------------
-- | The main point of the utility: grab video filenames from dir and  build a
--   StimulusSequence from them
work :: DirOpts -> IO StimulusSequence
work opt@DirOpts{..} = do

  files <- getDirectoryContents directory >>=
           filterM (doesFileExist . (directory </>))

  let fileGroups = groupBy ((==) `on` takeBaseName)
                   . sort
                   $ map takeFileName files :: [[FilePath]]

  stimSeqItems <- G.Array <$> traverse (getStimSeqItem opt) fileGroups

  return $ StimulusSequence (T.pack title) A.Null stimSeqItems (T.pack descr) (T.pack urlBase)


------------------------------------------------------------------------------
-- | Wrap a group of filenames as a StimSeqItem as long as they have
--   a valid extension
getStimSeqItem :: DirOpts -> [FilePath] -> IO StimSeqItem
getStimSeqItem DirOpts{..} fileGroup =
  let okFiles = filter ((`elem` map ("."<>) vidExtensions) . takeExtension)
                fileGroup
  in  return $ StimSeqItem (A.toJSON okFiles)


data SortBy = Name | Created | Modified
  deriving (Eq, Show, Read, Enum, Bounded)

vidExtensions :: [FilePath]
vidExtensions = ["mp4","ogv"]

data DirOpts = DirOpts
  { directory :: !String
  , sortBy    :: !SortBy
  , urlBase   :: !String
  , title     :: !String
  , descr     :: !String
  , dbCfg     :: !String
  , outFile   :: !String
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

  <$> (option str (long "path" <> short 'p'
                   <> help "Path to data directory") <|> pure ".")

  <*> (option auto (long "sort" <> short 's'
                   <> help ("Sort by [" ++ sr ++ "]")) <|> pure Name)

  <*> option str (long "url" <> short 'u' <> help "Base url")

  <*> option str (long "title" <> short 't' <> help "Title")

  <*> option str (long "description" <> short 'd' <> help "Description")

  <*> option str (long "config" <> short 'c'
                  <> help "Database snaplet config file")

  <*> (option str (long "output" <> short 'o' <> help "Output json file")
       <|> pure "./out.json")

  where sr = intercalate "|" (map show [minBound..(maxBound :: SortBy)])

fullOpts :: ParserInfo DirOpts
fullOpts = info (helper <*> dirOpts)
           (fullDesc
           <> progDesc fullDescription
           <> header "directoryToStimSet - a tool for Tagging")


main :: IO ()
main = do
  opts    <- execParser fullOpts
  stimSeq <- work opts
  print (outFile opts)
  BS.writeFile (outFile opts) (A.encode stimSeq)
  k <- dbInsert opts stimSeq
  print $ "Database key: " <> show k
  -- print (A.encode stimSeq)

dbInsert :: DirOpts -> StimulusSequence -> IO (AutoKey StimulusSequence)
dbInsert DirOpts{..} stimSeq = do
  cfg <- load [Required dbCfg]
  hostName <- require cfg "host"
  password <- require cfg "pass"
  dbName   <- require cfg "db"
  userName <- require cfg "user"
  let dbString = unwords ["user="     <> userName
                         ,"dbname="   <> dbName
                         ,"host="     <> hostName
                         ,"password=" <> password
                         ]
  withPostgresqlConn dbString $ runDbConn $ insert stimSeq


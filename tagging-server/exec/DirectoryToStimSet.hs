{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
import           Control.Monad                   (filterM)
import           Control.Monad.IO.Class          (liftIO)
import qualified Data.Aeson                      as A
import qualified Data.ByteString.Lazy.Char8      as BS
import           Data.Configurator
import           Data.Function                   (on)
import           Data.List                       (groupBy, intercalate, sort)
import           Data.Proxy
import qualified Data.Text                       as T
import qualified Data.UUID                       as U
import qualified Data.UUID.V4                    as U4
import qualified Data.UUID.V5                    as U5
import           Database.Groundhog.Postgresql
import           Database.Groundhog.Postgresql.Array as G
import           Options.Applicative
import           Data.String.QQ
import           System.Directory
import           System.FilePath
import           System.Environment
import           System.IO
import           Prelude                         hiding (FilePath)
------------------------------------------------------------------------------
import           Tagging.Stimulus
import           Server.Database
import           Server.Crud
import           Server.Utils
import           Server.Resources


------------------------------------------------------------------------------
-- | Mangle a filename by generating a UUID from the UUID of the parent
--   sequence and the filename (including extension, excluding path) of
--   the stimulus
--   e.g. : StimulusSequence parent's uuid is aa-bb-yy-zz,
--          Stimulus video is /Users/greghale/video-01.mp4,
--          hiddenName will be cc-dd-ww-xx.mp4
hiddenName :: StimulusSequence -> FilePath -> FilePath
hiddenName StimulusSequence{..} videoPath =
  let fullFilename = takeFileName  videoPath
      fullNameStr  = maybe (error "path conversion failure") id
                     (toString fullFilename)
      fullNameCode = map (fromIntegral . ord) fullNameStr
      hiddenBase   = U.toString $ U5.generateNamed ssUUID (fullNameCode)
      hiddenName   = fromString hiddenBase </> takeExtension videoPath

------------------------------------------------------------------------------
-- | The main point of the utility: grab video filenames from dir and  build a
--   StimulusSequence from them
work :: DirOpts -> IO (StimulusSequence, [StimSeqItem])
work opt@DirOpts{..} = do

  files <- getDirectoryContents directory >>=
           filterM (doesFileExist . (directory </>))

  let fileGroups = groupBy ((==) `on` takeBaseName)
                   . sort
                   $ map takeFileName files :: [[FilePath]]

  -- stimSeqItems <- G.Array <$> traverse (getStimSeqItem opt) fileGroups
  u <- U4.nextRandom
  let stimSeq = StimulusSequence (T.pack title) u
                                 A.Null (T.pack descr) (T.pack urlBase)
  ssItems <- traverse (getStimSeqItem opt) (zip [0..] fileGroups)

  return (stimSeq, ssItems)


------------------------------------------------------------------------------
-- | Wrap a group of filenames as a StimSeqItem as long as they have
--   a valid extension
getStimSeqItem :: DirOpts -> (Int, [FilePath]) -> IO StimSeqItem
getStimSeqItem DirOpts{..} (ind, fileGroup) =
  let okFiles = filter ((`elem` vidExtensions) . takeExtension)
                fileGroup
  in  return $ StimSeqItem (A.toJSON okFiles) 0 ind


-- TODO: Unused. Use or delete
data SortBy = Name | Created | Modified
  deriving (Eq, Show, Read, Enum, Bounded)

vidExtensions :: [FilePath]
vidExtensions = [".mp4",".ogv"]

data DirOpts = DirOpts
  { directory :: !String
  , sortBy    :: !SortBy -- TODO Unused. Use or remove
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
  (stimSeq, ssItems) <- work opts
  print (outFile opts)
  BS.writeFile (outFile opts) (A.encode stimSeq)

  withDB opts $ do
    ssKey <- insert stimSeq
    let p = Proxy :: Proxy StimulusSequence
    traverse insert
      ((\ssi -> ssi { ssiStimulusSequence = autoToInt p ssKey} ) <$> ssItems)
  return ()

-- TODO: Find out what type this actually is
-- withDB :: DirOpts -> (Postgresql  -> IO a) -> IO a
withDB DirOpts{..} act = do
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
  withPostgresqlConn dbString $ runDbConn act


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


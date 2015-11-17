{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

------------------------------------------------------------------------------
import           Control.Lens                        ((&),(?~))
import           Control.Monad                       (filterM)
import           Control.Monad.IO.Class              (liftIO)
import qualified Data.Aeson                          as A
import qualified Data.ByteString.Char8               as BSC
import qualified Data.ByteString.Lazy.Char8          as BS
import           Data.Configurator
import           Data.Function                       (on)
import           Data.List                           (groupBy, intercalate,
                                                      sort)
import           Data.Maybe                          (fromMaybe)
import           Data.Proxy
import qualified Data.Text                           as T
import qualified Data.UUID                           as U
import qualified Data.UUID.V4                        as U4
import qualified Data.UUID.V5                        as U5
import           Database.Groundhog.Postgresql
import           Database.Groundhog.Postgresql.Array as G
import qualified Network.Wreq                        as W
import           Options.Applicative
import           Data.String.QQ
import           System.Directory
import           System.FilePath
import           System.Environment
import           System.IO
import           Prelude                             hiding (FilePath)
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
      fullNameStr  = -- maybe (error "path conversion failure") id
                     (fullFilename)
      fullNameCode = map (fromIntegral . fromEnum) fullNameStr
      hiddenBase   = U.toString $ U5.generateNamed ssUuid (fullNameCode)
      hiddenName   = hiddenBase </> takeExtension videoPath
  in  hiddenName


------------------------------------------------------------------------------
-- | The main point of the utility: grab video filenames from dir and build a
--   StimulusSequence from them
mkStimSeq :: SetupOpts -> IO (StimulusSequence, [([FilePath],StimSeqItem)])
mkStimSeq opts@SetupOpts{..} = do

  files <- getDirectoryContents soPath >>=
           filterM (doesFileExist . (soPath </>))

  let fileGroups = groupBy ((==) `on` takeBaseName)
                   . sort
                   $ map takeFileName files :: [[FilePath]]

  u <- U4.nextRandom
  let stimSeq = StimulusSequence (T.pack soTitle) u
                                 A.Null (T.pack soDescr) (T.pack soUrlBase)
  ssItems <- traverse (getStimSeqItem opts) (zip [0..] fileGroups)

  return (stimSeq, zip fileGroups ssItems)


------------------------------------------------------------------------------
-- | Wrap a group of filenames as a StimSeqItem as long as they have
--   a valid extension
getStimSeqItem :: SetupOpts -> (Int, [FilePath]) -> IO StimSeqItem
getStimSeqItem SetupOpts{..} (ind, fileGroup) =
  let okFiles = filter ((`elem` vidExtensions) . takeExtension)
                fileGroup
  in  return $ StimSeqItem (A.toJSON okFiles) (intToKey Proxy 0) ind


-- TODO: Unused. Use or delete
data SortBy = Name | Created | Modified
  deriving (Eq, Show, Read, Enum, Bounded)

vidExtensions :: [FilePath]
vidExtensions = [".mp4",".ogv"]

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

data Opts = SOpts SetupOpts | DOpts DbOpts | UOpts UploadOpts

data SetupOpts = SetupOpts
  { soPath    :: !String
  , soSortBy  :: !SortBy
  , soUrlBase :: !String
  , soTitle   :: !String
  , soDescr   :: !String
  , soOutFile :: !String
}


data DbOpts = DbOpts
  { doSeqFile :: !FilePath
  , dbCfg     :: !String
  } deriving (Show)


data UploadOpts = UploadOpts
  { uoSeqFile :: FilePath
  , uoConfig  :: FilePath
  , uoBucket  :: FilePath
  } deriving (Show)


setupOpts :: Parser Opts
setupOpts = fmap SOpts $ SetupOpts
  <$> (option str (long "path" <> short 'p'
                   <> help "Path to data directory") <|> pure ".")
  <*> (option auto (long "sort" <> short 's'
                   <> help ("Sort by [" ++ sr ++ "]")) <|> pure Name)
  <*> option str (long "url" <> short 'u' <> help "Base url")
  <*> option str (long "title" <> short 't' <> help "Title")
  <*> option str (long "description" <> short 'd' <> help "Description")
  <*> (option str (long "output" <> short 'o' <> help "Output json file")
       <|> pure "./out.json")
  where sr = intercalate "|" (map show [minBound..(maxBound :: SortBy)])


dbOpts :: Parser Opts
dbOpts = fmap DOpts $ DbOpts
  <$> (option str (long "path" <> short 'p'
                  <> help "Setup file"))
  <*> option str (long "config" <> short 'c' <> help "DB snaplet config")


uploadOpts :: Parser Opts
uploadOpts = fmap UOpts $ UploadOpts
  <$> (option str (long "path" <> short 'p'
                   <> help "Path to data directory") <|> pure ".")
  <*> option str (long "config" <> short 'c'
                  <> help "Snaplet config file with AWS keys")
  <*> option str (long "bucket" <> short 'b'
                  <> help "S3 bucket name")


fullOpts :: ParserInfo Opts
fullOpts = info (helper <*>
                 (subparser
                  (command "setup"
                   (info setupOpts
                    (progDesc "Set up the stimulus sequence"))
                  <>
                  command "database"
                    (info dbOpts
                     (progDesc "Create stimulus database entries"))
                  <>
                  command "s3"
                    (info uploadOpts
                     (progDesc "Upload resources to s3")))))

           (fullDesc
           <> progDesc fullDescription
           <> header "directoryToStimSet - a tool for Tagging")


uploadOne :: UploadOpts
          -> W.Options
          -> StimulusSequence
          -> ([FilePath], StimSeqItem)
          -> IO Bool
uploadOne UploadOpts{..} wreqOpts sSeq (paths, ssItem) = do
  flip traverse paths $ \fp -> do
    picContents <- BSC.readFile fp
    r <- W.putWith wreqOpts
      (uoBucket <> ".s3.amazonaws.com")
      picContents -- TODO right url?
    print r -- TODO maybe get the response code out?
  return True


main :: IO ()
main = do
  let readStimSeq f = fmap (fromMaybe (error "Read Error") . A.decode) $ BS.readFile f
  opts    <- execParser fullOpts
  case opts of

    SOpts so@SetupOpts{..} -> do
      s <- mkStimSeq so
      BS.writeFile (soOutFile) (A.encode s)

    DOpts dOpts -> do
      (stimSeq :: StimulusSequence, ssItems) <- readStimSeq (doSeqFile dOpts)
      withDB dOpts $ do
        ssKey <- insert (stimSeq :: StimulusSequence)
        traverse insert
          ((\ssi -> ssi { ssiStimulusSequence = ssKey} ) <$>
           (map snd (ssItems :: [([FilePath],StimSeqItem)])))
        return ()

    UOpts uOpts -> do
      cfg <- load [Required (uoConfig uOpts)]
      kId <- require cfg "researcherid"
      key <- require cfg "researcherkey"
      let opts = W.defaults & W.auth ?~ (W.awsAuth W.AWSv4) kId key
      (stimSeq :: StimulusSequence, ssItems) <- readStimSeq (uoSeqFile uOpts)
      traverse (uploadOne uOpts opts stimSeq) (ssItems :: [([FilePath],StimSeqItem)])
      return ()


-- TODO: Find out what type this actually is
-- withDB :: DirOpts -> (Postgresql  -> IO a) -> IO a
withDB DbOpts{..} act = do
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


-- dbInsert :: DirOpts -> StimulusSequence -> IO (AutoKey StimulusSequence)
-- dbInsert DirOpts{..} stimSeq = do
--   cfg <- load [Required dbCfg]
--   hostName <- require cfg "host"
--   password <- require cfg "pass"
--   dbName   <- require cfg "db"
--   userName <- require cfg "user"
--   let dbString = unwords ["user="     <> userName
--                          ,"dbname="   <> dbName
--                          ,"host="     <> hostName
--                          ,"password=" <> password
--                          ]
--   withPostgresqlConn dbString $ runDbConn $ insert stimSeq


{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

------------------------------------------------------------------------------
import           Control.Concurrent                  (threadDelay)
import           Control.Lens                        ((&),(?~),(^.))
import           Control.Monad                       (filterM, when)
import           Control.Monad.Logger                (NoLoggingT)
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
import           Data.Traversable                    (for)
import qualified Data.UUID                           as U
import qualified Data.UUID.V4                        as U4
import qualified Data.UUID.V5                        as U5
import           Database.Groundhog.Postgresql
import qualified Network.Wreq                        as W
import           Options.Applicative
import           Data.String.QQ
import           System.Directory
import           System.FilePath
import           System.IO
import           Prelude                             hiding (FilePath)
------------------------------------------------------------------------------
import           Tagging.Stimulus
-- import           Server.Database
import           Server.Crud                         ()
import           Server.Utils
import           Server.Resources                    ()


------------------------------------------------------------------------------
-- | Respond to the CLI by either setting up a file with the StimulusSequence
--   and StimSeqItem info (what are the original filenames, what are the
--   StimSeqItems) | or writing those entries to the database | or uploading
--   those files with mangled names to S3 according to the s3 creds in a
--   snaplet config file
main :: IO ()
main = do
  let readStimSeq f = fromMaybe (error "Read Error") . A.decode <$> BS.readFile f
  opts    <- execParser fullOpts
  case opts of

    SOpts so@SetupOpts{..} -> do
      ss <- mkStimSeq so
      BS.writeFile soOutFile (A.encode ss)

    DOpts dOpts -> do
      (stimSeq :: StimulusSequence, ssItems) <- readStimSeq (doSeqFile dOpts)
      withDB dOpts $ do
        ssKey <- insert (stimSeq :: StimulusSequence)
        _ <- traverse insert
          ((\ssi -> ssi { ssiStimulusSequence = ssKey} ) <$>
           map snd (ssItems :: [([FilePath],StimSeqItem)]))
        return ()

    UOpts uOpts -> do
      cfg <- load [Required (uoConfig uOpts)]
      kId <- require cfg "researcherid"
      key <- require cfg "researcherkey"
      let s3Opts = W.defaults & W.auth ?~ W.awsAuth W.AWSv4 kId key
      (stimSeq :: StimulusSequence, ssItems) <- readStimSeq (uoSeqFile uOpts)
      _ <- traverse (uploadOne uOpts s3Opts stimSeq) (ssItems :: [([FilePath],StimSeqItem)])
      return ()


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
      fullNameCode = map (fromIntegral . fromEnum) fullFilename
      hiddenBase   = U.toString $ U5.generateNamed ssUuid fullNameCode
  in  hiddenBase <> takeExtension videoPath


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
                                 soSample
  ssItems <- traverse (getStimSeqItem opts stimSeq) (zip [0..] fileGroups)

  return (stimSeq, zip fileGroups ssItems)


------------------------------------------------------------------------------
-- | Wrap a group of filenames as a StimSeqItem as long as they have
--   a valid extension
getStimSeqItem :: SetupOpts -> StimulusSequence -> (Int, [FilePath]) -> IO StimSeqItem
getStimSeqItem SetupOpts{..} stimSeq (ind, fileGroup) =
  let okFiles = filter ((`elem` vidExtensions) . takeExtension)
                fileGroup
  in  return $ StimSeqItem (A.toJSON (map (hiddenName stimSeq) okFiles))
                           (intToKey Proxy 0)
                           ind


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
  , soSample  :: !SamplingMethod
  , soOutFile :: !String
}


data DbOpts = DbOpts
  { doSeqFile :: !FilePath
  , dbCfg     :: !String
  } deriving (Show)


data UploadOpts = UploadOpts
  { uoSeqFile :: FilePath
  , uoBaseDir :: FilePath
  , uoConfig  :: FilePath
  , uoBucket  :: FilePath
  } deriving (Show)


uploadOne :: UploadOpts
          -> W.Options
          -> StimulusSequence
          -> ([FilePath], StimSeqItem)
          -> IO Bool
uploadOne UploadOpts{..} wreqOpts sSeq (paths, ssItem) = do
  _ <- for paths $ \fp -> do
    picContents <- BSC.readFile (uoBaseDir <> fp)
    r <- W.putWith wreqOpts
      ("http://" <> uoBucket <> ".s3.amazonaws.com/" <> hiddenName sSeq fp)
      picContents
    when (r ^. W.responseStatus . W.statusCode /= 200) $ do
      putStrLn "Failed"
      print    ssItem
      putStrLn "Response:"
      print    r
    when (r ^. W.responseStatus . W.statusCode == 200) $
      putStrLn $ "Success on index " <> show (ssiIndex ssItem)
    threadDelay 200000 -- Wait half a second to to be bombarding the server
  return True


-- TODO: Find out what type this actually is
-- withDB :: DirOpts -> (Postgresql  -> IO a) -> IO a
withDB :: DbOpts -> DbPersist Postgresql (NoLoggingT IO) a -> IO a
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



setupOpts :: Parser Opts
setupOpts = fmap SOpts $ SetupOpts
  <$> (option str (long "path" <> short 'p'
                   <> help "Path to data directory") <|> pure ".")
  <*> (option auto (long "sort" <> short 's'
                   <> help ("Sort by [" ++ sr ++ "]")) <|> pure Name)
  <*> option str (long "url" <> short 'u' <> help "Base url")
  <*> option str (long "title" <> short 't' <> help "Title")
  <*> option str (long "description" <> short 'd' <> help "Description")
  <*> (option auto (long "sampling" <> short 'n' <> help "Sampling Method")
       <|> pure SampleIncrement)
  <*> (option str (long "output" <> short 'o' <> help "Output json file")
       <|> pure "./out.json")
  where sr = intercalate "|" (map show [minBound..(maxBound :: SortBy)])


dbOpts :: Parser Opts
dbOpts = fmap DOpts $ DbOpts
  <$> option str (long "path" <> short 'p'
                 <> help "Setup file")
  <*> option str (long "config" <> short 'c' <> help "DB snaplet config")


uploadOpts :: Parser Opts
uploadOpts = fmap UOpts $ UploadOpts
  <$> (option str (long "seqfile" <> short 's'
                   <> help "Path to data directory") <|> pure ".")
  <*> option str (long "base" <> short 'b'
                  <> help "Base path of stimuli")
  <*> option str (long "config" <> short 'c'
                  <> help "Snaplet config file with AWS keys")
  <*> option str (long "bucket" <> short 'b'
                  <> help "S3 bucket name")


fullOpts :: ParserInfo Opts
fullOpts = info (helper <*>
                 subparser
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
                     (progDesc "Upload resources to s3"))))

           (fullDesc
           <> progDesc fullDescription
           <> header "directoryToStimSet - a tool for Tagging")


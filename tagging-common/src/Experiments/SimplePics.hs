{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Experiments.SimplePics where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import Data.Typeable
import Database.Groundhog
import Database.Groundhog.Postgresql
import System.FilePath ((</>))
import Tagging.Database
import Tagging.Stimulus


data SimplePics

instance Experiment SimplePics where
  type Stimulus SimplePics = String -- Path to picture
  type Question SimplePics = OneToTen
  type Answer   SimplePics = Int

  -- | To get a stimulus from a resource for SimplePics,
  --   just return the url of the picture
  getResource = \StimulusResource{..} -> return $
    T.unpack srUrlSuffix </> T.unpack srName

picsSt = "Simple Pictures"

data PrefsRange = PrefsRange { rangeMin :: Int, rangeMax :: Int }

data OneToTen = OneToTen { oneToTenScore :: Int }
  deriving (Eq, Show)

-- TODO is this right?
toTypeName :: Typeable t => Proxy t -> T.Text
toTypeName p = T.pack . show $ typeRep p


------------------------------------------------------------------------------
setupStimuli :: DbPersist Postgresql (NoLoggingT IO)
                  (Key StimulusSequence BackendSpecific)
setupStimuli = do

  let stims = ['a'..'c']
  liftIO $ print "About to make resource entities"
  resourceEntities <- forM stims $ \c -> do
    let r = StimulusResource (T.pack [c]) (T.pack $ c : ".jpg") "image/jpeg"
    k <- insert r
    return (k,r)

  let stimSeq = StimulusSequence
                { ssName        = picsSt
                , ssFirstItem   = Nothing
                , ssDescription = "Minimal framework test"
                , ssBaseUrl     = "web.mit.edu/greghale/Public/pics"
                }

  liftIO $ print "About to import stimSeq"
  seqKey <- insert stimSeq
  liftIO $ print "About to zipWith"
  let seqItems = zipWith f [0.. length resourceEntities - 1] resourceEntities
        where f i (k,r) =
                   StimSeqItem
                   { ssiStimSeq = seqKey
                   , ssiStimulus = k
                   , ssiNextItem = Nothing
                   , ssiIndex    = i
                   , ssiResponseType = toTypeName (Proxy :: Proxy OneToTen)
                   }
  liftIO $ print "About to addStimSeq"
  i1 <- addStimulusSequence seqKey stimSeq seqItems
  liftIO $ print "All done"
  return i1


-- stimuli :: [StimulusResource]
-- stimuli = map picStim ['a'..'c']
--  where picStim x = StimulusResource
--                      { srName = T.pack [x]
--                      , srUrlSuffix = T.pack $ x : ".jpg"
--                      , srMimeType  = "image/jpeg"
--                      }

-- seqItems :: [StimSeqItem]
-- seqItems = zipWith f [0..] stimuli
--   where f i s = StimSeqItem { ssiStimSeq = undefined}

-- sequenceSpec :: (StimSeqName, [(StimulusName, ResponseType)])
-- sequenceSpec = (picsSt, stims)
--   where stims = flip map stimuli $ \StimulusResource{..} ->
--                       (srName, toTypeName (Proxy :: Proxy OneToTen))

-- initializeDB :: String -> Bool -> IO ()
-- initializeDB dbString mig = withPostgresqlConn dbString $ runDbConn $ do
--   when mig $ runMigration $ do
--     migrate (undefined :: StimulusResource)
--     migrate (undefined :: StimSeqItem)
--     migrate (undefined :: StimulusSequence)
--   setKey   <- insert stimSeq
--   zipWithM_ (\i stim -> do
--     stimResource <- insert stim
--     insert $ StimSeqItem setKey stimResource Nothing i
--              (toTypeName (Proxy :: Proxy OneToTen))) [0..] stimuli

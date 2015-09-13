{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Server.Experiments.HomeAlone where

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
import Server.Database
import Tagging.Stimulus
import Experiments.HomeAlonePersonAndDirection
import Server.Resources
import Server.Utils


------------------------------------------------------------------------------
setupStimuli :: DbPersist Postgresql (NoLoggingT IO)
                  (Key StimulusSequence BackendSpecific)
setupStimuli = do

  let stims = ["lego","holder","big_buck_bunny_480p"]
  liftIO $ print "About to make Home Alone resource entities"
  resourceEntities <- forM stims $ \c -> do
    let r = StimulusResource (T.pack c) (T.pack $ c <> ".ogv") "video/ogv"
    k <- insert r
    return (k,r)

  let stimSeq = StimulusSequence
                { ssName        = "HomeAlone2"
                , ssFirstItem   = Nothing
                , ssDescription = "Home Alone videos"
                , ssBaseUrl     = "http://web.mit.edu/greghale/Public/testvids"
                }

  liftIO $ print "About to import Home Alone stimSeq"
  seqKey <- insert stimSeq
  liftIO $ print "About to zipWith"
  -- TODO UUID's as keys, please
  let seqItems = zipWith f [4.. length resourceEntities - 1 + 4] resourceEntities
        where f i (k,r) =
                   StimSeqItem
                   { ssiStimSeq = keyToInt seqKey
                   , ssiStimulus = keyToInt k
                   , ssiNextItem = Nothing
                   , ssiIndex    = i
                   , ssiResponseType = "[CharacterAtDir]"
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

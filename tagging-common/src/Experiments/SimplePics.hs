{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Experiments.SimplePics where

import Control.Monad
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import Data.Typeable
import Database.Groundhog.Postgresql
import System.FilePath ((</>))
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

stimuli :: [StimulusResource]
stimuli = map picStim ['a'..'c']
 where picStim x = StimulusResource
                     { srName = T.pack [x]
                     , srUrlSuffix = T.pack $ x : ".jpg"
                     , srMimeType  = "image/jpeg"
                     }

stimSet :: StimSeq
stimSet = StimSeq
          { ssName        = picsSt
          , ssDescription = "Minimal framework test"
          , ssBaseUrl     = "web.mit.edu/greghale/Public/pics"
          }

sequenceSpec :: (StimSeqName, [(StimulusName, ResponseType)])
sequenceSpec = (picsSt, stims)
  where stims = flip map stimuli $ \StimulusResource{..} ->
                      (srName, toTypeName (Proxy :: Proxy OneToTen))

data PrefsRange = PrefsRange { rangeMin :: Int, rangeMax :: Int }

data OneToTen = OneToTen { oneToTenScore :: Int }
  deriving (Eq, Show)

-- TODO is this right?
toTypeName :: Typeable t => Proxy t -> T.Text
toTypeName p = T.pack . show $ typeRep (undefined `asTypeOf` p)

initializeDB :: String -> Bool -> IO ()
initializeDB dbString mig = withPostgresqlConn dbString $ runDbConn $ do
  when mig $ runMigration $ do
    migrate (undefined :: StimulusResource)
    migrate (undefined :: StimSeqItem)
    migrate (undefined :: StimSeq)
  setKey   <- insert stimSet
  zipWithM_ (\i stim -> do
    stimResource <- insert stim
    insert $ StimSeqItem setKey stimResource i
             (toTypeName (Proxy :: Proxy OneToTen))) [0..] stimuli

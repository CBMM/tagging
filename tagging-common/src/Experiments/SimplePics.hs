{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Experiments.SimplePics where

import Control.Monad
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import Data.Typeable
import Database.Groundhog.Postgresql
import Tagging.Stimulus

picsSt = "Simple Pictures"

stimuli :: [StimulusResource]
stimuli = map picStim ['a'..'c']
 where picStim x = StimResource { srName = T.pack [x]
                                , srUrlSuffix = T.pack $ x : ".jpg"
                                , srMimeType  = "image/jpeg"
                                }

stimSet :: StimulusSet
stimSet = StimSet { ssName        = picsSt
                  , ssDescription = "Minimal framework test"
                  , ssBaseUrl     = "web.mit.edu/greghale/Public/pics"
                  }

sequenceSpec :: (StimSetName, [(StimName, ResponseType)])
sequenceSpec = (picsSt, stims)
  where stims = flip map stimuli $ \StimResource{..} ->
                      (srName, toTypeName (Proxy :: Proxy OneToTen))

data OneToTen = OneToTen { oneToTenScore :: Int }
  deriving (Eq, Show)

-- TODO is this right?
toTypeName :: Typeable t => Proxy t -> T.Text
toTypeName p = T.pack . show $ typeRep (undefined `asTypeOf` p)

initializeDB :: String -> Bool -> IO ()
initializeDB dbString mig = withPostgresqlConn dbString $ runDbConn $ do
  when mig $ runMigration $ do
    migrate (undefined :: StimulusResource)
    migrate (undefined :: StimulusSequenceItem)
    migrate (undefined :: StimulusSet)
  setKey   <- insert stimSet
  zipWithM_ (\i stim -> do
    stimResource <- insert stim
    insert $ StimSeqItem setKey stimResource i
             (toTypeName (Proxy :: Proxy OneToTen))) [0..] stimuli

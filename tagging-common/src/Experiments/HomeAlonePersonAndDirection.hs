{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Experiments.HomeAlonePersonAndDirection where

import Data.Monoid
import qualified Data.Text as T
import Tagging.Stimulus

clipSet :: StimSeq
clipSet =
  StimSeq "HomeAloneClips" "Invididual shots from Home Alone 2"
  "/static/clips/HomeAlone2"

clips :: [StimulusResource]
clips = map (\n -> StimulusResource (T.pack (show n))
                    ("clip" <> T.pack (show n) <> ".mp4")
         "video/mp4") [0..10]


data HomeAloneExperiment

instance Experiment HomeAloneExperiment where
  type Stimulus HomeAloneExperiment = Int
  type Question HomeAloneExperiment = Int
  type Answer   HomeAloneExperiment = String

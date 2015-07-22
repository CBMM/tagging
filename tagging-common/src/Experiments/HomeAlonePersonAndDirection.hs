{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module Experiments.HomeAlonePersonAndDirection where

import Tagging.Stimulus

clipSet :: StimulusSet
clipSet =
  StimSet "HomeAloneClips" "Invididual shots from Home Alone 2"
  "/static/clips/HomeAlone2"

clips :: [StimulusResource]
clips = map (\n -> StimResource ("clip" <> show n <> ".mp4") "video/mp4")
        [0..10]


data HomeAloneExperiment

instance IsTrial HomeAloneExperiment where
  type Stimulus HomeAloneExperiment = Int
  type Question HomeAloneExperiment = Int
  type Answer   HomeAloneExperiment = String

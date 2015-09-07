{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ExistentialQuantification #-}

module Tagging.Experiments.HomeAlone.Widgets where

import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Functor
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Time
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Aeson as A
import           Data.Default
import           GHC.Int
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Time
import           Reflex.Dom.Xhr

import           Tagging.Response
import           Tagging.Stimulus
import           Tagging.User
import           Experiments.HomeAlonePersonAndDirection


-----------------------------------------------------------------------------
pageWidget :: forall t m .MonadWidget t m => TaggingUser -> m ()
pageWidget TaggingUser{..} = mdo

  pb <- getPostBuild
  let getStim = leftmost [pb]

  stims   <- fmapMaybe id <$> getAndDecode ("/api/posinfo" <$ getStim)

  elClass "div" "question-div" $
        movieWidget stims

  qWidget <- questionWidget stims :: m (Dynamic t String)

  return ()


-----------------------------------------------------------------------------
questionWidget :: MonadWidget t m
               => Event t PositionInfo
               -> m (Dynamic t (Answer HomeAloneExperiment))
questionWidget p = do

  pb <- getPostBuild

  fakeClicks <- button "Incr"
  mapDyn (show :: Int -> String) =<< count fakeClicks


-----------------------------------------------------------------------------
movieWidget :: MonadWidget t m => Event t PositionInfo -> m ()
movieWidget pEvent = do

  let movieSrc   = \PositionInfo{..} ->
                     ssBaseUrl (snd piStimulusSequence) <> "/"
                     <> srUrlSuffix (snd piStimulusResource)
      movieAttrs = \p -> "src"  =: movieSrc p
                      <> "type" =: srMimeType (snd $ piStimulusResource p)

  movieAttrs <- widgetHold (text "waiting")
    (ffor pEvent $ \p ->
      elAttr "video" ("width" =: "320"
                      <> "height" =: "240"
                      <> "controls" =: "controls") $ do
      elAttr "source" (Map.map T.unpack $ movieAttrs p)
        (return ())
    )

  return ()

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

  searchBox <- _textInput_value <$> textInput def

  optionBankWidget searchBox

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

-----------------------------------------------------------------------------
-- A listing of all possible faces, filtered by text typed so far
optionBankWidget :: MonadWidget t m => Dynamic t String -> m ()
optionBankWidget searchString = elClass "div" "bank-container" $ do
  listViewWithKey (constDyn choicesMap) (oneChoiceWidget searchString)
  return ()


-----------------------------------------------------------------------------
-- Dom components for a single face with name (no direction info)
oneChoiceWidget :: MonadWidget t m
                => Dynamic t String
                -> String
                -> Dynamic t String
                -> m (Event t ())
oneChoiceWidget searchString n dynPath = elClass "div" "bank-item" $ do
  divAttrs <- combineDyn
              (\s p -> let isIn  = T.toLower (T.pack s)
                                   `T.isInfixOf`
                                   T.toLower (T.pack n)
                           style = bool "opacity: 0.4" "opacity: 1.0" isIn
                       in "src" =: p
                          <> "style" =: style
                          <> "class" =: "one-choice") searchString dynPath
  elDynAttr "div" divAttrs $ do
    imgAttrs <- mapDyn ("src" =:) dynPath

    elDynAttr "img" imgAttrs $ return ()
    dynSearchAct <- searchText searchString n
    dyn dynSearchAct
    return never -- TODO fix

searchText :: MonadWidget t m
           => Dynamic t String
           -> String
           -> m (Dynamic t (m ()))
searchText query source = do

  let qSource = T.pack source
  qText <- mapDyn (T.toLower . T.pack) query
  dynAction <-
    forDyn qText $ \q ->
      case not (T.null q) && T.toLower q `T.isInfixOf` T.toLower qSource of
        False -> el "h2" $ text source
        True  ->
          let breakPoint = T.length . fst . T.breakOn q . T.toLower $ qSource
              (p0,pTemp) = T.splitAt breakPoint qSource
              (p1,p2)    = T.splitAt (T.length q) pTemp
          in el "h2" $ do
            text (T.unpack p0)
            elClass "span" "text-found" $ text (T.unpack p1)
            text (T.unpack p2)
  return dynAction


-----------------------------------------------------------------------------
-- Listing of names and paths to their pics (hard-coded for now. TODO serve)
choicesMap :: Map.Map String String
choicesMap = Map.fromList $ map (\n -> (n, nameToFile n))
             ["Kevin McC" ,"Tracy McC" ,"Sondra McC" ,"Rod McC" ,"Rob McC"
             ,"Buzz McC" ,"Peter McC" ,"Other" ,"Other (Major)" ,"Other (Minor)"
             ,"Not Sure" ,"Nobody" ,"Mrs. Stone" ,"Mr. Hector" ,"Mr. Duncan"
             ,"Megan McC" ,"Marv Merch" ,"Linnie McC" ,"Leslie McC" ,"Kate McC"
             ,"Jeff McC" ,"Harry Lyme" ,"Fuller McC" ,"Frank McC" ,"Cedric"
             ,"Buzz McC" ,"Brooke McC" ,"Bird Lady"
             ]

nameToFile :: String -> String
nameToFile = ("http://web.mit.edu/greghale/Public/hapics/" <>)
             . (<> ".png")
             . filter (`notElem` ("() ." :: String))

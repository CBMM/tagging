{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Tagging.Experiments.SimplePics.Widgets where

import           Control.Error
import           Control.Monad
import           Data.Functor
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Aeson as A
import           Data.Default
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Time
import           Reflex.Dom.Xhr

import           Tagging.Response
import           Tagging.Stimulus
import           Tagging.User
import           Experiments.SimplePics


pageWidget :: forall t m.MonadWidget t m => TaggingUser -> m ()
pageWidget TaggingUser{..} = mdo

  postBuild  <- getPostBuild
  submits    <- button "Submit"
  nextClicks <- delay 0.5 submits

  el "br" $ return ()

  let getStim = leftmost [postBuild, nextClicks]

  dynSequence <- holdDyn (Nothing :: Maybe StimulusSequence)
                 =<< getAndDecode (postBuild $> "/api/sequence")
  dynResource <- holdDyn (Nothing :: Maybe StimulusResource)
                 =<< getAndDecode (getStim   $> "/api/resource")

  text "DynSequence: "
  display dynSequence

  el "br" $ return ()

  text "DynResource"
  display dynResource

  el "br" $ return ()

  let getStimUrl = "/api/resource" :: String
      putRespUrl = "/api/response" :: String

  stimResourceEvents <- getAndDecode (getStimUrl <$ getStim)
  let stimEvents = fmap stimulusWidget (fforMaybe stimResourceEvents id)
  _ <- widgetHold (text "waiting") stimEvents

  ansTime <- performEvent (getCurrentTime <$ submits)

  answer <- questionWidget
  stimResponse <- mapDyn toResponse answer ansTime
  display stimResponse

  let toAnswerReq v =
       XhrRequest "POST" "/api/response"
        (XhrRequestConfig (Map.fromList [("Content-Type","application/json")])
                           Nothing Nothing Nothing (Just . BSL.unpack $ A.encode v))
  let respRequests = fmap toAnswerReq (tag (current stimResponse) submits)

  ansResp <- performRequestAsync respRequests

  el "br" $ return ()
  display =<< holdDyn "No sends" (fmap show ansResp)

  return ()

toResponse :: UTCTime -> Answer SimplePics -> StimulusResponse
toResponse t n = StimulusResponse 0 0 t t "NoType?" (T.decodeUtf8 . BSL.toStrict $ A.encode n)

stimulusWidget :: MonadWidget t m => StimulusResource -> m ()
stimulusWidget StimulusResource{..} = mdo
  elAttr "img"
    ("class" =: "stim"
     <> "src" =: ("http://web.mit.edu/greghale/Public/pics/"
                  <> T.unpack srUrlSuffix))
    (return ())
  return ()

stimulusWidget' :: MonadWidget t m => Maybe StimulusResource -> m ()
stimulusWidget' Nothing = text "Got Nothing"
stimulusWidget' (Just StimulusResource{..}) = mdo
  elAttr "img"
    ("class" =: "stim"
     <> "src" =: ("http://web.mit.edu/greghale/Public/pics/"
                  <> T.unpack srUrlSuffix))
    (return ())
  return ()


questionWidget :: MonadWidget t m
               => m (Dynamic t (Either String (Answer SimplePics)))
questionWidget = mdo
  v <- mapDyn (validate <=< readErr "Not a number")
       =<< _textInput_value <$> textInput def
  return v
  where validate n | n >= 0 && n < 10 = Right (n :: Int)
                   | otherwise        = Left "Must be between 0 an 10"

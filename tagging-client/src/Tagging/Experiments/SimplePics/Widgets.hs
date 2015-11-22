{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Tagging.Experiments.SimplePics.Widgets where

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
import           Experiments.SimplePics

{-
pageWidget :: forall t m.MonadWidget t m => TaggingUser -> m ()
pageWidget TaggingUser{..} = mdo

  postBuild  <- getPostBuild
  submits    <- button "Submit"
  nextClicks <- delay 0.5 submits

  el "br" $ return ()

  let getStim = leftmost [postBuild, nextClicks]

  dynStim <- holdDyn (Nothing :: Maybe PositionInfo)
             =<< getAndDecode ("/api/posinfo" <$ getStim)

  text "DynSequence: "
  display dynStim

  el "br" $ return ()

  _ <- widgetHold (text "waiting")
        $ fmap stimulusWidget (fmapMaybe id $ updated dynStim)

  ansTime <- holdDyn (UTCTime (fromGregorian 2015 1 1) 0)
             =<< performEvent (liftIO getCurrentTime <$ submits)

  answer <- questionWidget

  display answer

  let toAnswerReq (Right v) =
       Just $ XhrRequest "POST" "/api/response"
        (XhrRequestConfig (Map.fromList [("Content-Type","application/json")])
                           Nothing Nothing Nothing
                           (Just . BSL.unpack $ A.encode (ResponsePayload (T.decodeUtf8 . BSL.toStrict $ A.encode v))))
      toAnswerReq (Left _) = Nothing
  let respRequests = fforMaybe (fmap toAnswerReq (tag (current answer) submits)) id

  ansResp <- performRequestAsync respRequests

  el "br" $ return ()
  el "br" $ text "RespRequest:"
  display =<< holdDyn "No sends" (fmap show respRequests)

  el "br" $ return ()
  el "br" $ text "answResp:"
  display =<< holdDyn "No sends" (fmap show ansResp)

  return ()

stimulusWidget :: MonadWidget t m => PositionInfo -> m ()
stimulusWidget (PositionInfo ss ssi sr) = mdo
  elAttr "img"
    ("class" =: "stim"
     <> "src" =: (T.unpack (ssBaseUrl . snd $ ss) <> "/"
                  <> T.unpack (srUrlSuffix . snd $ sr)))
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
-}

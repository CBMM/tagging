{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Admin where

import Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Aeson as A
import GHC.Generics
import Reflex.Dom


-- TODO Use correct types for start & stop indices
data HomeAloneHit = HAHit
  { haStart :: String
  , haStop  :: String
  , haCode  :: String
  } deriving (Show, Generic, A.ToJSON, A.FromJSON)


adminWidget :: MonadWidget t m => m ()
adminWidget = mdo
  showClicks <- button "Admin Dialog"
  showAdmin <- holdDyn True $ leftmost [True <$ showClicks, False <$ closeClicks]
  dialogProps <- forDyn showAdmin $ bool mempty ("open" =: "true")
  closeClicks <- elDynAttr "dialog" dialogProps adminDialogContents
  return ()


adminDialogContents :: MonadWidget t m => m (Event t ())
adminDialogContents = do

  start <- elClass "div" "inp-group" $ do
    text "Start index:"
    fmap value $ textInput def

  stop <- elClass "div" "inp-group" $ do
    text "Stop index:"
    fmap value $ textInput def

  code  <- elClass "div" "inp-group" $ do
    text "Survey code:"
    fmap value $ textInput def

  hit <- HAHit `mapDyn` start `apDyn` stop `apDyn` code
  url <- mapDyn makeUrl hit

  elClass "div" "instructions" $ text copyingInstructions
  el "br" (return ())
  elClass "pre" "dynlink" $ dynText url

  closeEvents <- button "close"
  return closeEvents


makeUrl :: HomeAloneHit -> String
makeUrl ha = "http://tagging.codes/api/turk/?data=" ++ encodeHit ha


encodeHit :: HomeAloneHit -> String
encodeHit h = BSL.unpack . B64.encode $ A.encode h

copyingInstructions :: String
copyingInstructions =
  "Copy this URL into the survey link on mechanical turk. " ++
  "Be sure to keep a record of the 'code' so that you can " ++
  "verify people reached the end of the stimulus block when"++
  " they submit their survey."

apDyn :: MonadWidget t m
      => m (Dynamic t (a -> b))
      -> Dynamic t a
      -> m (Dynamic t b)
apDyn mf a = do
  f <- mf
  combineDyn ($) f a

{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Aeson as A
import Data.Bool
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Foldable
import Data.Monoid
import qualified Data.Text as T
import Data.Traversable (forM)
import GHC.Generics
import Reflex
import Reflex.Dom
import Tagging.Response
import Tagging.Stimulus
import Tagging.User

mimeOf :: String -> String
mimeOf fn = case extension fn of
  "oog" -> "video/oog"
  "mp4" -> "video/mp4"
  where extension = reverse . takeWhile (/= '.') . reverse

data StimResponse = StimResponse
  { _respRemember :: Bool }
  deriving (Eq, Ord, Show, Generic)

instance ToJSON StimResponse
instance FromJSON StimResponse

run :: forall t m.MonadWidget t m => m ()
run = mdo
  pb <- getPostBuild

  let stimRequestTriggers = leftmost [pb, () <$ submitSuccess]
  posTry <- getAndDecode ("/api/fullposinfo" <$ stimRequestTriggers)

  let pos :: Event t (Assignment, StimulusSequence, StimSeqItem) = fmapMaybe id posTry
  errorText (text "Failed to load position")
            (True <$ ffilter (== Nothing) posTry)
  responses :: Dynamic t (Event t StimResponse) <- elClass "div" "interaction" $
    widgetHold (text "Waiting..." >> return never) (fmap videoQuestion pos)
  submitSuccess <- performRequestAsync
    (ffor (switchPromptlyDyn responses) $ \(r :: StimResponse) ->
      XhrRequest "POST" "/api/response?advance" $
      XhrRequestConfig ("Content-Type" =: "application/json")
      Nothing Nothing Nothing (Just . BSL.unpack $ A.encode
                               (ResponsePayload (A.toJSON r))))

  return ()


videoQuestion :: forall t m.MonadWidget t m
              => (Assignment, StimulusSequence, StimSeqItem)
              -> m (Event t StimResponse)
videoQuestion (asgn, stimseq, ssi) = do
  elClass "div" "videoandquestion" $ do
    vid <- elAttr' "video" ("height"   =: "320"
                         <> "width"    =: "240"
                         <> "controls" =: "true") $ do
      case ssiStimulus ssi of
        A.Array fileNames -> do
          forM fileNames $ \(A.String fn) -> do
              elAttr "src" ("src" =: T.unpack fn
                         <> "type" =: mimeOf (T.unpack fn)) fin
          text "Sorry, this browser does not support our videos"
        _ -> text "Stimulus decoding error"
    rememb <- elAttr "div" ("class" =: "yesno") $ do
       ys <- fmap (True  <$) (button "Yes")
       ns <- fmap (False <$) (button "No")
       return $ leftmost [ys,ns]
    remembered <- holdDyn Nothing $ fmap Just rememb
    return $ fmap StimResponse $ fmapMaybe id (updated remembered)

welcome :: MonadWidget t m => m (Event t ())
welcome = elClass "div" "welcome" $ do
  text $ unwords ["Thank you for participating in this experiment."
                 ,"Please watch the following video clips,"
                 ,"and after each one, indicate whether you saw this"
                 , "clip during the first phase of the experiment."]
  el "br" fin
  text $ "If you feel uncomfortable, you may stop the experiment at any time."
  text $ "Logging in later will bring you to your last spot in the experiment."
  button "Continue"

main :: IO ()
main = mainWidget run

fin :: MonadWidget t m => m ()
fin = return ()

-- | Throwaway error-reporting widget
errorText :: MonadWidget t m => m () -> Event t Bool -> m ()
errorText msg setShowing = mdo
  showing <- holdDyn False $ leftmost [setShowing, False <$ closeClick]
  divAttr <- forDyn showing $ \s -> "class"   =: "error-text"
                                 <> "display" =: bool "none" "block" s
  closeClick <- elDynAttr "div" divAttr $ do
    msg
    (e,_) <- elAttr' "span" ("class" =: "glyphicon glyphicon-remove") fin
    return $ domEvent Click e
  fin


module Main where

import Control.Monad (mzero)
import Reflex
import Reflex.Dom

data StimResponse = Resp { _respHaveSeen :: Bool }
  deriving (Eq, Ord, Show, Generic)

run :: MonadWidget t m => m ()
run = do
  pb <- getPostBuild

  let stimRequestTriggers = leftmost [() <$ pb, () <$ submitSuccess]
  pos <- getAndDecode ("/api/fullposinfo" <$ stimRequestTriggers)
  responses <- elClass "div" "interaction" $
    dyn

videoQuestion :: MonadWidget m t
              => (Assignment, StimulusSequence, StimSeqItem)
              -> m (Event t ())
videoQuestion = do
  elClass "div" "video" $ do
    el "video" $ do
      forM (ssi)
      elDynAttr "src"


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


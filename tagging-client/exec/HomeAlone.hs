{-# LANGUAGE RecursiveDo #-}

module Main where

import Reflex
import Reflex.Dom
import Tagging.Stimulus
import Tagging.User
import Tagging.Experiments.HomeAlone.Widgets

-- main :: IO ()
-- main = mainWidget
--        (text "Hello tagging!" >> pageWidget
--          (TaggingUser 0 Nothing Nothing (Just 0) [Subject]))

main :: IO ()
main = mainWidget contentWidget

contentWidget :: MonadWidget t m => m ()
contentWidget = elClass "div" "content" $ mdo
  elClass "div" "movie-widget" (text "movie")
  elClass "div" "selections-widget" (text "selections")
  elClass "div" "clip-properties-widget" (text "clip props")
  elClass "div" "stable-properties-widget" (text "stable props")

  toggleCharEvents <- elClass "div" "choice-bank-widget"
                      (choiceBankWidget choices (constDyn []))

  return ()

{-# LANGUAGE RecursiveDo #-}

module Main where

import qualified Data.Set as Set
import Reflex
import Reflex.Dom
import Tagging.Stimulus
import Tagging.User
import Tagging.Experiments.HomeAlone.Widgets


main :: IO ()
main = mainWidget contentWidget

contentWidget :: MonadWidget t m => m ()
contentWidget = elClass "div" "content" $ mdo

  elClass "div" "movie-widget" (text "movie")
  elClass "div" "selections-widget" (text "selections")
  elClass "div" "clip-properties-widget" (text "clip props")

  -- Stable Properties
  stableUpdates <- elClass "div" "stable-properties-widget" $
                   (stablePropsWidget mempty (updated currentSingleSel))

  -- Choice Bank
  toggleCharEvents <- elClass "div" "choice-bank-widget"
                      (choiceBankWidget choices (constDyn []))

  dynSelection <- foldDyn (\toggleChar (_,s) ->
                            if Set.member toggleChar s
                            then (Nothing,         Set.delete toggleChar s)
                            else (Just toggleChar, Set.insert toggleChar s)
                           )
                  (Nothing, Set.empty)
                   toggleCharEvents

  currentSingleSel <- mapDyn fst dynSelection
  currentSetSel    <- mapDyn snd dynSelection

  display currentSetSel
  display currentSingleSel

  return ()

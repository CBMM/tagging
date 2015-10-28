{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Data.Bool (bool)
import Reflex
import Reflex.Dom
import Tagging.Stimulus
import Tagging.User
import Tagging.Experiments.HomeAlone.Widgets


main :: IO ()
main = mainWidget contentWidget

contentWidget :: MonadWidget t m => m ()
contentWidget = elClass "div" "content" $ mdo

  posEvents <- getPostBuild
  posInfos <- fmapMaybe id <$> getAndDecode ("/api/posinfo" <$ posEvents)

  (selClicks, delClicks, submits, togClicks) <- elClass "div" "top-half" $ do
      (sc, dc, sub) <- elClass "div" "top-left" $ do
        elClass "div" "movie-widget" (movieWidget posInfos)
        elClass "div" "selections-widget" $
          selectionsWidget currentSetSel
      togs <- elClass "div" "choice-bank-widget"
                (choiceBankWidget choices (constDyn []))
      return (sc, dc, sub, togs)

  stableUpdates <- elClass "div" "bottom-half" $ do
      elClass "div" "clip-properties-widget"
        (clipPropsWidget (updated currentSingleSel) submits)
      stUps <- elClass "div" "stable-properties-widget" $
                 (stablePropsWidget mempty (updated currentSingleSel))
      return stUps

  let foldAux (toggleChar, forceDirection) (_,s) =
        let ifIns = (Just toggleChar, s ++ [toggleChar])
            ifNot = (Nothing, filter (/= toggleChar) s)
        in (case forceDirection of
             Just b  -> bool ifNot ifIns b
             Nothing -> bool ifIns ifNot (toggleChar `elem` s))
  sel <- foldDyn foldAux (Nothing, []) (leftmost $ [fmap (,Nothing) togClicks])

  currentSingleSel <- mapDyn fst sel
  currentSetSel    <- mapDyn snd sel

  return ()

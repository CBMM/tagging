{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Aeson as A
import           Data.Bool (bool)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.List as List
import qualified Data.Map  as Map
import           Data.Maybe (catMaybes, isJust, maybeToList)
import           Data.Monoid ((<>))
import           Reflex
import           Reflex.Dom
import           Experiments.HomeAlonePersonAndDirection
import           Tagging.Stimulus
import           Tagging.Response
import           Tagging.User
import           Tagging.Experiments.HomeAlone.Widgets


main :: IO ()
main = mainWidget contentWidget

-------------------------------------------------------------------------------
contentWidget :: MonadWidget t m => m ()
contentWidget = elClass "div" "content" $ mdo

  postBuild <- getPostBuild
  let fetchEvents = postBuild <> (() <$ clipXhr)
  posEvents <- (getAndDecode ("/api/fullposinfo" <$ fetchEvents))

  {-  Left Half -}

  (selWidget, clipUps, nOthers) <- elClass "div" "left-half" $ mdo

    elClass "div" "left-child" $ do
      elClass "div" "movie-widget" (movieWidget (fmapMaybe id posEvents))

    (selWidget, nOthers) <- elClass "div" "left-child" $ do
      elClass "div" "selections-widget" $
        selectionsWidget currentSetSel currentSingleSel okToSend

    clipUps <- elClass "div" "left-child properties" $ do
                 elClass "div" "clip-properties-widget" $
                   clipPropsWidget choices (updated currentSingleSel)
                   (swSends selWidget)

    return (selWidget, clipUps, nOthers)

  {-  Right Half -}

  (togs, stableUps) <- elClass "div" "right-half" $ mdo

    togs <- elClass "div" "righc-child choice-bank-widget"
              (choiceBankWidget choices (constDyn []))

    stUps <- elClass "div" "stable-properties-widget" $
               (stablePropsWidget choices (updated currentSingleSel))

    return (togs,stUps)


  {- Logic -}

  sel <- foldDyn foldAux (Nothing, [])
         (leftmost [ fmap ToggleSelect      togs
                   , fmap InsertAndSelect   (swAdditions selWidget)
                   , fmap DeleteAndDeselect (swDeletions selWidget)
                   , ClearAllSelected <$ clipXhr])

  currentSingleSel :: Dynamic t (Maybe CharacterName) <- holdDyn Nothing (leftmost [(fmap fst $ updated sel)
                                                                                   ,Nothing <$ clipXhr])
  currentSetSel    :: Dynamic t [CharacterName]       <- holdDyn [] (leftmost [fmap snd (updated sel)
                                                                              ,[] <$ clipXhr])

  clipProps <- sampleClipProperties clipUps currentSetSel
  okToSend  <- combineDyn (\cp selSet -> length cp == length selSet)
               clipProps currentSetSel
  charsAndOthers <- combineDyn (,) clipProps nOthers
  let clipResponses =
        ffor (gate (current okToSend) (tag (current charsAndOthers) (swSends selWidget))) $
          \(cp,n) -> XhrRequest "POST" "/api/response?advance" $
                       XhrRequestConfig ("Content-Type" =: "application/json")
                       Nothing Nothing Nothing (Just . BSL.unpack $ A.encode
                                               (ResponsePayload (A.toJSON (PerClip (cp,n)))))
  clipXhr <- performRequestAsync clipResponses

  return ()


data SelectionUpdate
  = InsertAndSelect   CharacterName
  | DeleteAndDeselect CharacterName
  | ToggleSelect      CharacterName
  | ClearAllSelected

-- | Update the current point selection and selection set
--   according to toogling clicks on character names
foldAux :: SelectionUpdate
        -> (Maybe CharacterName, [CharacterName])
        -> (Maybe CharacterName, [CharacterName])
foldAux (InsertAndSelect   nm) (_, oldSet) = (Just nm, List.union [nm] oldSet)
foldAux (DeleteAndDeselect nm) (_, oldSet) = (Nothing, List.delete nm oldSet)
foldAux ClearAllSelected       _           = (Nothing, [])
foldAux (ToggleSelect      nm) (s, oldSet)
  | s == Just nm = (Nothing, List.delete nm   oldSet)
  | otherwise    = (Just nm, List.union  [nm] oldSet)


-- | Sample the clip properties of all characters, at the curretly-selected
--   characters. This is the data to send to the server on "Submit" events.
sampleClipProperties :: MonadWidget t m
                     => ClipPropsMap t -- ^Mapping from character to dynamic (Maybe ClipProps)
                     -> Dynamic t [CharacterName] -- ^Current selection set
                     -> m (Dynamic t [ClipProperties])
sampleClipProperties clipPropsMap selectionSet = do
  let dyns = Map.elems clipPropsMap
  dynsAsLists  <- mapM (mapDyn maybeToList) dyns
  dynPropsList <- mconcatDyn dynsAsLists
  dynsInSet    <- combineDyn
                  (\selSet dynPropsList -> filter (\ClipProperties{..} -> _cpCharacterName `elem` selSet) dynPropsList)
                  selectionSet dynPropsList
  return dynsInSet

  -- joinDyn $ forDyn $ \selSet -> do
  --   dynJustProps <- mapM (mapDyn ) Map.elems $ Map.filter (`elem` selSet) clipPropsMap
  --   dynList <- mapM (mapDyn (\maybeProps)) ()
  --   mconcatDyn dynList


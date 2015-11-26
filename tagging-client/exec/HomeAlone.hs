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

contentWidget :: MonadWidget t m => m ()
contentWidget = elClass "div" "content" $ mdo

  postBuild <- getPostBuild
  let fetchEvents = postBuild <> (() <$ clipXhr) <> (() <$ stableUpdates)
  posEvents <- (getAndDecode ("/api/fullposinfo" <$ fetchEvents))

  (selClicks, delClicks, submits, togClicks) <- elClass "div" "top-half" $ do
      (sc, dc, sub) <- elClass "div" "top-left" $ do
        elClass "div" "movie-widget" (movieWidget (fmapMaybe id posEvents))
        elClass "div" "selections-widget" $
          selectionsWidget currentSetSel okToSend
      togs <- elClass "div" "choice-bank-widget"
                (choiceBankWidget choices (constDyn []))
      return (sc, dc, sub, togs)

  (clipUpdates, stableUpdates) <- elClass "div" "bottom-half" $ do
      clUps <- elClass "div" "clip-properties-widget"
        (clipPropsWidget choices (updated currentSingleSel) submits)
      stUps <- elClass "div" "stable-properties-widget" $
                 (stablePropsWidget choices (updated currentSingleSel))
      return (clUps, stUps)


  sel <- foldDyn foldAux (Nothing, [])
         (traceEventWith show $ leftmost [fmap (, Nothing)    togClicks
                   ,fmap (, Just True)  (traceEvent "sel" selClicks)
                   ,fmap (, Just False) (traceEvent "del" delClicks)])

  currentSingleSel :: Dynamic t (Maybe CharacterName) <- mapDyn fst sel
  currentSetSel    :: Dynamic t [CharacterName]       <- mapDyn snd sel

  clipProps <- sampleClipProperties clipUpdates currentSetSel
  okToSend  <- combineDyn (\cp selSet -> length cp == length selSet)
               clipProps currentSetSel
  let clipResponses =
        ffor (gate (current okToSend) (tag (current clipProps) submits)) $
        \cp -> XhrRequest "POST" "/api/response" $
               XhrRequestConfig ("Content-Type" =: "application/json")
               Nothing Nothing Nothing (Just . BSL.unpack $ A.encode
                                        (ResponsePayload (A.toJSON (PerClip cp))))
  clipXhr <- performRequestAsync clipResponses


  text "currentSetSel: "
  display currentSetSel
  el "br" $ return ()
  text "currentSingleSel: "
  display currentSingleSel
  el "br" $ return ()
  text "CurrentProps: "
  display clipProps

  return ()


-- | Update the current point selection and selection set
--   according to toogling clicks on character names
foldAux :: (CharacterName, Maybe Bool)  -- ^Toggled name; Toggle | Force Ins | Force Del
        -> (Maybe CharacterName ,[CharacterName]) -- ^Old point selection & set selection
        -> (Maybe CharacterName, [CharacterName]) -- ^New point selection & set selection
foldAux (charName, maybeDirection) (singleSel, selSet) =

  let direction = case maybeDirection of
        Nothing    -> not (charName `elem` selSet)
        Just True  -> True
        Just False -> False

      newCharacterSelection
          -- If inserting, single selection is the insertion
        | direction                                   = Just charName
          -- If deleting and name matches, delete selection
        | not direction && singleSel == Just charName = Nothing
          -- No other cases require a change
        | otherwise                                   = singleSel

      newSetSelection
        | direction = List.union [charName] selSet
        | otherwise = List.delete charName selSet

  in  (newCharacterSelection, newSetSelection)



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


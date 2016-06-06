{-# language OverloadedStrings #-}

module Main where

import Data.Text
import MovieCharacters.MetaData
import MovieCharacters.Widgets
import Reflex
import Reflex.Dom

exampleQ = QuestionSpec True "head" "Head"
  (QRadioChoice [Choice "headleft" "Left" ,Choice "headright" "Right"])

eQ2 = QuestionSpec False "gend" "Gender"
  (QButtonsChoice [Choice "gF" "Female", Choice "gM" "Male"])


main = mainWidget run

run :: MonadWidget t m => m ()
run = do
  el "br" (return ())
  cs <- metaDataWidget def
  md <- holdDyn (def :: MetaData) (fmapMaybe hush (updated cs))
  el "br" (return ())

  characters <- mapDyn characters md
  characterBank <- characterSelectionBank (CharacterSelectionBankConfig [] (updated characters))
  display $ _characterSelectionBank_characters characterBank

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _ = Nothing

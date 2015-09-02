module Main where

import Data.Proxy
import Reflex
import Reflex.Dom
import Tagging.User
import Tagging.Stimulus
import Tagging.Crud

main = mainWidget $ do
  text "StimulusResource Table"
  crudTableWidget (Proxy :: Proxy StimulusResource) (constDyn (const True))
  text "TaggingUser Table"
  crudTableWidget (Proxy :: Proxy TaggingUser) (constDyn (const True))
  --text "TaggingUser Table"
  --crudTableWidget (Proxy :: Proxy TaggingUser) (constDyn (const True))
  return ()

module Main where

import Data.Proxy
import Reflex
import Reflex.Dom
import Tagging.User
import Tagging.Stimulus
import Tagging.Crud

main = mainWidget $ do
  el "hr" $ return ()
  text "StimulusResource Table"
  crudTableWidget (Proxy :: Proxy StimulusResource) (constDyn (const True))
  el "br" $ return ()

  el "hr" $ return ()
  text "TaggingUser Table"
  crudTableWidget (Proxy :: Proxy TaggingUser) (constDyn (const True))
  el "br" $ return ()

  el "hr" $ return ()
  text "StimulusSequence Table"
  crudTableWidget (Proxy :: Proxy StimulusSequence) (constDyn (const True))
  el "br" $ return ()

  el "hr" $ return ()
  text "StimSeqItem Table"
  crudTableWidget (Proxy :: Proxy StimSeqItem) (constDyn (const True))
  el "br" $ return ()
  --text "TaggingUser Table"
  --crudTableWidget (Proxy :: Proxy TaggingUser) (constDyn (const True))
  return ()

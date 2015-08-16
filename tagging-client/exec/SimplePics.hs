module Main where

import Reflex
import Reflex.Dom
import Database.Groundhog
import Database.Groundhog.Postgresql
import Database.Groundhog.Core
import Tagging.Stimulus
import Tagging.User
import Tagging.Experiments.SimplePics.Widgets

main :: IO ()
main = mainWidget (pageWidget (TaggingUser 0 Nothing Nothing (Just (StimSeqItemKey (PersistInt64 0))) [Subject]))

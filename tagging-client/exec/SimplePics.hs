module Main where

import Reflex
import Reflex.Dom
import Tagging.Stimulus
import Tagging.User
import Tagging.Experiments.SimplePics.Widgets

main :: IO ()
main = mainWidget (pageWidget (TaggingUser 0 Nothing Nothing (Just 0) [Subject]))

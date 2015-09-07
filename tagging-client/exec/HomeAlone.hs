module Main where

import Reflex
import Reflex.Dom
import Tagging.Stimulus
import Tagging.User
import Tagging.Experiments.HomeAlone.Widgets

main :: IO ()
main = mainWidget
       (text "Hello tagging!" >> pageWidget
         (TaggingUser 0 Nothing Nothing (Just 0) [Subject]))

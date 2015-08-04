{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}

module Tagging.Experiments.SimplePics.Widgets where

import           Control.Error
import           Control.Monad
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import qualified Data.Aeson as A
import           Data.Default
import           Reflex
import           Reflex.Dom

import           Tagging.Stimulus
import           Tagging.User
import           Experiments.SimplePics

pageWidget :: MonadWidget t m => TaggingUser -> m ()
pageWidget TaggingUser{..} = mdo

  be <- getPostBuild


stimulusWidget :: MonadWidget t m
               => TaggingUser
               -> Stimulus SimplePics
               ->  m ()
stimulusWidget TaggingUser{..} stim = mdo

  elAttr "img" ("class" =: "stim" <> "src" =: stim) (return ())
  return ()


questionWidget :: MonadWidget t m => TaggingUser -> Question SimplePics
               -> m (Dynamic t (Either String (Answer SimplePics)))
questionWidget TaggingUser{..} q = mdo
  v <- mapDyn (validate <=< readErr "Not a number")
       =<< _textInput_value <$> textInput def
  return v
  where validate n | n >= 0 && n < 10 = Right (n :: Int)
                   | otherwise        = Left "Must be between 0 an 10"

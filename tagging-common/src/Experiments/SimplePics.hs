{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Experiments.SimplePics where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import Data.Typeable
import System.FilePath ((</>))
import Tagging.Stimulus


data SimplePics

instance Experiment SimplePics where
  type Stimulus SimplePics = String -- Path to picture
  type Question SimplePics = OneToTen
  type Answer   SimplePics = Int


picsSt = "Simple Pictures"

data PrefsRange = PrefsRange { rangeMin :: Int, rangeMax :: Int }

data OneToTen = OneToTen { oneToTenScore :: Int }
  deriving (Eq, Show)

-- TODO is this right?
toTypeName :: Typeable t => Proxy t -> T.Text
toTypeName p = T.pack . show $ typeRep p

{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections             #-}

module Tagging.Experiments.HomeAlone.Widgets where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Functor
import qualified Data.List                  as L
import qualified Data.Map                   as Map
import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.Lazy.Encoding    as TL
import           Data.Time
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Aeson                 as A
import           Data.Default
import           GHC.Int
------------------------------------------------------------------------------
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.Widgets.ButtonGroup
import           Reflex.Dom.Contrib.Widgets.Common
import           Reflex.Dom.Time
import           Reflex.Dom.Xhr
------------------------------------------------------------------------------
import           Tagging.Response
import           Tagging.Stimulus
import           Tagging.User

-- redacted for experimenter

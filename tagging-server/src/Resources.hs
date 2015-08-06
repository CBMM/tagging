{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Resources where

import Control.Error
import qualified Data.Text as T
import Database.Groundhog
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Groundhog.Postgresql
import Tagging.Stimulus
import Tagging.User
import Utils
import Application


getUserSequence :: EitherT String (Handler App App) [StimSeqItem]
getUserSequence = do
  TaggingUser{..}  <- noteT "TaggingUser lookup error" getCurrentTaggingUser
  seqElemKey       <- noteT "Usassigned" (hoistMaybe tuCurrentStimulus)
  StimSeqItem{..}  <- noteT "Bad StimSeqItem lookup"
                      . MaybeT . gh $ get seqElemKey
  EitherT $ fmap Right . gh $ select (SsiStimSeqField ==. ssiStimSeq)

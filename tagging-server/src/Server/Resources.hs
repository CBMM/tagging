{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Resources where

-----------------------------------------------------------------------------
import Control.Error
import qualified Data.Text as T
import Database.Groundhog
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Groundhog.Postgresql
import Tagging.Stimulus
import Tagging.User
-----------------------------------------------------------------------------
import Server.Utils
import Server.Application
import Server.Crud

instance Crud TaggingUser where

instance Crud StimulusResource where

instance Crud StimSeqItem where

-----------------------------------------------------------------------------
getUserSequence :: EitherT String (Handler App App) [StimSeqItem]
getUserSequence = do
  TaggingUser{..}  <- noteT "TaggingUser lookup error" getCurrentTaggingUser
  seqElemKey       <- noteT "Usassigned" (hoistMaybe tuCurrentStimulus)
  StimSeqItem{..}  <- noteT "Bad StimSeqItem lookup"
                      . MaybeT . gh $ get seqElemKey
  EitherT $ fmap Right . gh $ select (SsiStimSeqField ==. ssiStimSeq)

-----------------------------------------------------------------------------
getAllUsers :: Handler App App [(AutoKey TaggingUser, TaggingUser)]
getAllUsers = gh $ selectAll

{-# LANGUAGE TypeFamilies #-}

module Tagging.Database where

import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import Database.Groundhog
import Tagging.Stimulus

------------------------------------------------------------------------------
addStimulusSequence
  :: (MonadIO b, PersistBackend b)
  => Key StimulusSequence BackendSpecific
  -> StimulusSequence
  -> [StimSeqItem]
  -> b (AutoKey StimulusSequence)
addStimulusSequence seqKey seq [] = insert seq
addStimulusSequence seqKey seq (x:xs) = do
  liftIO $ print "About to insert first item:"
  liftIO $ print x
  itemKey0 <- insert (x   :: StimSeqItem)
  liftIO $ print "About to replace"
  replace seqKey (seq {ssFirstItem = Just itemKey0} :: StimulusSequence)
  liftIO $ print "About to enter Go loop"
  go itemKey0 xs
  liftIO $ print "About to return"
  return seqKey
  where go parentKey []     = do
          liftIO $ print "go loop terminal case"
          return ()
        go parentKey (x:xs) = do
          liftIO $ putStrLn ("Inserting " ++ show x)
          k  <- insert x
          v0 <- get parentKey
          maybe (error "Database insertion error")
            (\v0' -> replace parentKey (v0' {ssiNextItem = Just k}))
            v0
          go k xs

{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Server.Database where

import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B8
import Database.Groundhog
import Database.Groundhog.TH
import Tagging.Stimulus
import Tagging.User
import Tagging.Response


mkPersist defaultCodegenConfig [groundhog|
definitions:
  - entity: StimulusResource
  - entity: StimulusSequence
    keys:
      - name: SsName
    constructors:
      - name: StimulusSequence
        uniques:
          - name: SsName
            fields: [ssName]
  - entity: StimSeqItem
|]

mkPersist defaultCodegenConfig [groundhog|
  - entity: TaggingUser
    keys:
      - name: TuId
    constructors:
      - name: TaggingUser
        uniques:
          - name: TuId
            fields: [tuId]
  - entity: Role
    constructors:
      - name: Admin
      - name: Subject
      - name: Researcher
|]


mkPersist defaultCodegenConfig [groundhog|
  - entity: StimulusResponse
|]

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

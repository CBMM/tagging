{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module APIDocs where

import qualified Data.Text as T
import Data.Time
import Database.Groundhog.Core
import GHC.Int
import Servant
import Servant.Docs

import Tagging.Response
import Tagging.Stimulus
import Tagging.User
import API
import Server.Database
import Server.Session
import Server.Crud

apiDocs :: Servant.Docs.API
apiDocs = docs apiProxy

docsIntro = DocIntro "Welcome"
            ["Tagging-server api", "For all your stimulus tagging needs."]

instance ToSample Bool Bool where
  toSample _ = Just True

instance ToSample Int64 Int64 where
  toSample _ = Just 12

------------------------------------------------------------------------------
instance ToSample TaggingUser TaggingUser where
  toSample _ = Just sampleTaggingUser

instance ToSample [TaggingUser] [TaggingUser] where
  toSample _ = Just [sampleTaggingUser]

instance ToSample [(Int64,TaggingUser)] [(Int64,TaggingUser)] where
  toSample _ = Just [(1,sampleTaggingUser)]


sampleTaggingUser = TaggingUser 1 (Just "922763745") (Just "Greg Hale")
                    Nothing [Admin, Researcher, Subject]


instance ToSample StimSeqItem StimSeqItem where
  toSample _ = Just sampleStimSeqItem

instance ToSample [StimSeqItem] [StimSeqItem] where
  toSample _ = Just [sampleStimSeqItem]

instance ToSample [(Int64,StimSeqItem)] [(Int64,StimSeqItem)] where
  toSample _ = Just [(1,sampleStimSeqItem)]

sampleStimSeqItem =
  StimSeqItem ((1))
  ((1))
  (Just $ ((3))) 1 "Preference"

instance ToSample StimulusSequence StimulusSequence where
  toSample _ = Just sampleSequence

instance ToSample [StimulusSequence] [StimulusSequence] where
  toSample _ = Just [sampleSequence]

instance ToSample [(Int64,StimulusSequence)] [(Int64,StimulusSequence)] where
  toSample _ = Just [(1,sampleSequence)]

sampleSequence =
  StimulusSequence "SimplePictures" (Just ((1))) "Three pictures of shapes" "http://web.mit.edu/greghale/Public/shapes"

instance ToSample StimulusResource StimulusResource where
  toSample _ = Just sampleResource

instance ToSample [StimulusResource] [StimulusResource] where
  toSample _ = Just [sampleResource]

instance ToSample [(Int64, StimulusResource)] [(Int64, StimulusResource)] where
  toSample _ = Just [(0,sampleResource)]

sampleResource = StimulusResource "a" "a.jpg" "image/jpeg"

instance ToSample StimulusResponse StimulusResponse where
  toSample _ = Just sampleResponse

instance ToSample [StimulusResponse] [StimulusResponse] where
  toSample _ = Just [sampleResponse]

instance ToSample [(Int64, StimulusResponse)] [(Int64, StimulusResponse)] where
  toSample _ = Just [(0,sampleResponse)]

sampleResponse =
  StimulusResponse
  ((1))
  ((1))
  (UTCTime (fromGregorian 2015 08 21) 0)
  (UTCTime (fromGregorian 2015 08 21) 1)
  "SimplePicturePreference (todo fix)"
  "10"

instance ToSample () () where
  toSample _ = Just ()

instance ToCapture (Capture "id" Int64) where
  toCapture _ = DocCapture "id" "Resource id number"

instance ToParam (QueryParam "username" T.Text) where
  toParam _ = DocQueryParam "username"
               ["Alan","Alonzo", "..."]
               "Login name on the site"
               Normal

instance ToParam (QueryParam "password" T.Text) where
  toParam _ = DocQueryParam "password"
               ["cryptobean","1amdba", "..."]
               "User password on the site"
               Normal

instance ToParam (QueryFlag "remember") where
  toParam _ = DocQueryParam "remember"
               ["True", "False"]
               "Keep a session logged in"
               Flag

instance ToParam (QueryParam "realname" T.Text) where
  toParam _ = DocQueryParam "realname"
               ["Alan Turing","Alonzo Church", "..."]
               "(Optional) Full name"
               Normal

instance ToParam (QueryParam "studentid" T.Text) where
  toParam _ = DocQueryParam "studentid"
               ["","123-456-789", "..."]
               "(Optional) Full name"
               Normal

instance HasDocs (Raw a b) where
  docsFor _ _ = mempty

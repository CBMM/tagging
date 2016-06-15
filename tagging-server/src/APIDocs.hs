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
import Tagging.API
import Server.Database
import Server.Session
import Server.Crud

apiDocs :: Servant.Docs.API
apiDocs = undefined -- docs apiProxy

docsIntro = DocIntro "Welcome"
            ["Tagging-server api", "For all your stimulus tagging needs."]

-- instance ToSample Bool Bool where
--   toSample _ = Just True

instance ToSample Int64 where
  toSamples _ = singleSample 12

-- instance ToSample () () where
--   toSample _ = Just ()

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

instance ToParam (QueryFlag "advance") where
  toParam _ = DocQueryParam "advance" []
              ("Set this flag to advance when submitted " ++
              "stim response should advance the trial index")
              Normal


instance ToSample Char where
  toSamples _ = singleSample 'c'
-- instance HasDocs (Raw a b) where
--   docsFor _ _ = mempty

instance ToSample Int where
  toSamples _ = singleSample 1


instance ToParam (QueryParam "experiment" Int) where
  toParam _ = DocQueryParam "experiment" []
    ("Experiment (stimulus sequence) ID of the answer key to look up") Normal

instance ToParam (QueryParam "extra-data" String) where
  toParam _ = DocQueryParam "extra-data" []
    ("Metadata string from turk experiment") Normal



instance ToParam (QueryParam "index" [Int]) where
  toParam _ = DocQueryParam "index" []
    ("Stimulus id's to be looking up (any id's not answered " ++
     "by the user will be ignored)") Normal


instance ToCapture (Capture "sequence" Int64) where
  toCapture _ = DocCapture "sequence" "Key for the Stimulus Sequence"

instance ToCapture (Capture "userid" Int64) where
  toCapture _ = DocCapture "sequence" "Numeric key for the user"

instance ToSample LoginInfo where
  toSamples _ = singleSample (LoginInfo "greg" "myPassword" True)


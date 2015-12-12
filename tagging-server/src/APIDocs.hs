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

module Tagging.User where

import qualified Data.Text as T

data User = User {
  authName :: T.Text
}

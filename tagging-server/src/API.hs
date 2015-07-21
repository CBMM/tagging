{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module API where

import Servant
import Servant.Server
import Tagging.User
import Tagging.Stimulus

-- type API =
--        "stim" :> Capture "set" Int :> Capture "item" Int
--               :> Get '[JSON] Stimulus
--   :<|> "resp" :> Capture "set" Int :> Capture "item" Int
--               :> Post '[JSON] Response

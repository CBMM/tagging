{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Server.Application where

------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import qualified Database.Groundhog.Postgresql as G
import qualified Snap.Snaplet.Groundhog.Postgresql as G
import Snap.Snaplet.PostgresqlSimple

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager App)
    , _db    :: Snaplet Postgres
    , _gdb   :: Snaplet G.GroundhogPostgres
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasPostgres (Handler b App) where
   getPostgresState = with db get
   setLocalPostgresState s = local (set (db . snapletValue) s)

instance G.HasGroundhogPostgres (Handler b App) where
  getGroundhogPostgresState = with gdb get

------------------------------------------------------------------------------
type AppHandler = Handler App App

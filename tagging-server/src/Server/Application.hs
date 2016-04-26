{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Server.Application where

------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Data.Pool
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import qualified Database.Groundhog as G
import qualified Database.Groundhog.Core as G
import qualified Database.Groundhog.Postgresql as G
-- import Snap.Snaplet.PostgresqlSimple

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager App)
--    , _db    :: Snaplet Postgres
    , _gh   ::  Pool    G.Postgresql
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

-- instance HasPostgres (Handler b App) where
--    getPostgresState = with db get
--    setLocalPostgresState s = local (set (db . snapletValue) s)

-- instance G.HasGroundhogPostgres (Handler b App) where
--   getGroundhogPostgresState = with gdb get

-- instance G.ConnectionManager (Pool G.Postgresql) G.Postgresql where
--   withConn f pconn = withResource pconn $ G.withConn f . G.Postgresql
--   withConnNoTransaction f pconn =
--      withResource pconn $ G.withConnNoTransaction f . G.Postgresql

instance G.ConnectionManager App G.Postgresql where
  withConn f app              = G.withConn f (_gh app)
  withConnNoTransaction f app = G.withConnNoTransaction
                                 f (_gh app)

runGH :: G.ConnectionManager b conn
      => G.DbPersist conn (NoLoggingT IO) a
      -> Handler b v a
runGH f = withTop' id $ do
  cm <- ask
  liftIO $ runNoLoggingT (G.withConn (G.runDbPersist f) cm)

------------------------------------------------------------------------------
type AppHandler = Handler App App

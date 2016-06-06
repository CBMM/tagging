{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Utils
  ( keyToInt
  , keyToIntegral
  , intToKey
  , integralToKey
  , ghCodeGen
-- #ifndef __GHCJS__
--   , ghConfig
-- #endif
  ) where

------------------------------------------------------------------------------
import           Data.Pool
import           Database.Groundhog
import           Database.Groundhog.Core
import           Database.Groundhog.Generic
import           Database.Groundhog.TH
#ifndef __GHCJS__
import           Database.Groundhog.Postgresql
#else
import           GHC.Int
#endif
------------------------------------------------------------------------------
import qualified Data.UUID.Types as U


-- #ifdef __GHCJS__

-- type family DefaultKey a :: *

-- #else

#ifndef __GHCJS__
pg :: proxy Postgresql
pg = undefined



#else

data NilBackend

instance DbDescriptor NilBackend where
  type AutoKeyType NilBackend = Int64
  type QueryRaw    NilBackend = []
  backendName _    = "NilBackend"

pg :: proxy NilBackend
pg = undefined
#endif


-------------------------------------------------------------------------------
keyToInt
    :: (PrimitivePersistField (Key a b))
    => Key a b
    -> Int
keyToInt = keyToIntegral


-------------------------------------------------------------------------------
-- | Convert 'Key' to any integral type.
keyToIntegral
    :: (PrimitivePersistField i, PrimitivePersistField (Key a b))
    => Key a b
    -> i
keyToIntegral =
    fromPrimitivePersistValue pg . toPrimitivePersistValue pg


-------------------------------------------------------------------------------
-- | Type specialized input for type inference convenience.
intToKey
    :: (PrimitivePersistField (Key a b))
    => Int
    -> Key a b
intToKey = integralToKey


-------------------------------------------------------------------------------
-- | Convert any integral type to 'Key'
integralToKey
    :: (PrimitivePersistField i, PrimitivePersistField (Key a b))
    => i
    -> Key a b
integralToKey =
    fromPrimitivePersistValue pg . toPrimitivePersistValue pg


instance PersistField U.UUID where
  persistName _ = "UUID"
  toPersistValues = primToPersistValue
  fromPersistValues = primFromPersistValue
  dbType _ _ = DbTypePrimitive DbString False Nothing Nothing

instance PrimitivePersistField U.UUID where
  toPrimitivePersistValue p u = toPrimitivePersistValue p (show u)
  fromPrimitivePersistValue p x = read (fromPrimitivePersistValue p x)

ghCodeGen :: CodegenConfig
ghCodeGen = defaultCodegenConfig { namingStyle = lowerCaseSuffixNamingStyle }

ghConfig :: CodegenConfig
ghConfig = defaultCodegenConfig
    { namingStyle = lowerCaseSuffixNamingStyle }


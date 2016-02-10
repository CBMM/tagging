{ mkDerivation, aeson, base, bytestring, configurator, containers
, directory, errors, filepath, groundhog, groundhog-postgresql
, groundhog-th, heist, http-types, lens, map-syntax, monad-logger
, MonadCatchIO-transformers, mtl, optparse-applicative
, postgresql-simple, resource-pool, s3-signer, servant
, servant-docs, servant-foreign, servant-js, servant-matlab
, servant-snap, SHA, snap, snap-core, snap-loader-dynamic
, snap-loader-static, snap-server, snaplet-postgresql-simple
, stdenv, string-qq, tagging-common, text, time, transformers
, unordered-containers, uuid, wreq
}:


mkDerivation {
  pname = "tagging-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;


  libraryHaskellDepends = [
    aeson base bytestring configurator containers errors filepath
    groundhog groundhog-postgresql groundhog-th heist http-types lens
    map-syntax monad-logger MonadCatchIO-transformers mtl
    postgresql-simple resource-pool servant servant-docs servant-snap
    SHA snap snap-core snap-loader-dynamic snap-loader-static
    snap-server snaplet-postgresql-simple tagging-common text time
    transformers unordered-containers uuid
  ];
  executableHaskellDepends = [
    aeson base bytestring configurator containers directory errors
    filepath groundhog groundhog-postgresql groundhog-th heist
    http-types lens map-syntax monad-logger MonadCatchIO-transformers
    mtl optparse-applicative postgresql-simple resource-pool s3-signer
    servant servant-docs servant-foreign servant-js servant-matlab
    servant-snap snap snap-core snap-loader-dynamic snap-loader-static
    snap-server snaplet-postgresql-simple string-qq tagging-common text
    time transformers unordered-containers uuid wreq
  ];
  license = stdenv.lib.licenses.bsd3;
}

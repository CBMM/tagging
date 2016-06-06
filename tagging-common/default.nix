{ mkDerivation, aeson, base, base64-bytestring, bytestring, deepseq
, errors, filepath, groundhog, groundhog-postgresql, groundhog-th
, lens, monad-logger, resource-pool, servant, servant-docs, stdenv
, text, time, transformers, uuid-types, vector, compilername ? "ghcjs"
}:
mkDerivation {
  pname = "tagging-common";
  version = "0.1.0.0";
  isLibrary = true;
  src = ./.;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring deepseq errors filepath
    groundhog groundhog-th lens monad-logger
    resource-pool servant servant-docs text time transformers uuid-types
    vector
  ] ++ (if compilername == "ghc" then [ groundhog-postgresql ] else [ ]);
  license = stdenv.lib.licenses.bsd3;
}

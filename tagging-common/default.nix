{ mkDerivation, aeson, base, base64-bytestring, bytestring, deepseq
, errors, filepath, groundhog, groundhog-postgresql, groundhog-th
, lens, monad-logger, resource-pool, servant, servant-docs, stdenv
, text, time, transformers, uuid, vector
}:
mkDerivation {
  pname = "tagging-common";
  version = "0.1.0.0";
  isLibrary = true;
  src = ./.;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring deepseq errors filepath
    groundhog groundhog-postgresql groundhog-th lens monad-logger
    resource-pool servant servant-docs text time transformers uuid
    vector
  ];
  license = stdenv.lib.licenses.bsd3;
}

{ mkDerivation, aeson, base, bytestring, containers, ghcjs-dom
, reflex, reflex-dom, stdenv, tagging-common, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "videomemory";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers ghcjs-dom reflex reflex-dom
    tagging-common text unordered-containers vector
  ];
  license = stdenv.lib.licenses.bsd3;
}

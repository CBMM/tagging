{ mkDerivation, aeson, base, bytestring, containers, ghcjs-dom
, lens, lens-aeson, reflex, reflex-dom, reflex-dom-contrib, stdenv
, tagging-common, text, unordered-containers, vector
}:
mkDerivation {
  pname = "homealone";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers ghcjs-dom 
    lens lens-aeson reflex reflex-dom reflex-dom-contrib
    tagging-common text unordered-containers vector
  ];
  license = stdenv.lib.licenses.bsd3;
}

{ mkDerivation, aeson, base, bytestring, containers, data-default
, errors, ghcjs-dom, groundhog, groundhog-th, reflex, reflex-dom, reflex-dom-contrib
, stdenv, tagging-common, text, time
, transformers, uuid, yaml-ghcjs
}:
mkDerivation {
  pname = "tagging-client";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers data-default errors ghcjs-dom
    groundhog groundhog-th reflex reflex-dom
    tagging-common text time transformers uuid yaml-ghcjs
  ];
  executableHaskellDepends = [
    aeson base bytestring containers ghcjs-dom groundhog reflex
    reflex-dom reflex-dom-contrib tagging-common text
  ];
  license = stdenv.lib.licenses.bsd3;
}

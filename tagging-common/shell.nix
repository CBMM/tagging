{ compilername, nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, base64-bytestring, bytestring
      , deepseq, errors, filepath, groundhog, groundhog-postgresql
      , groundhog-th, lens, monad-logger, resource-pool, servant
      , servant-docs, stdenv, text, time, transformers, uuid, vector
      }:
      mkDerivation {
        pname = "tagging-common";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson base base64-bytestring bytestring deepseq errors filepath
          groundhog groundhog-th lens monad-logger
          resource-pool servant servant-docs text time transformers uuid
          vector
        ] ++ (if compilername == "ghc" then [ groundhog-postgresql ] else [ ]);
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv

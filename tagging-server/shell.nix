{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;
  dontCheck = (import <nixpkgs> {}).pkgs.haskell.lib.dontCheck;
  myrun = pkg: dontCheck (pkgs.callPackage (cabal2nixResult pkg) {});


  # lifted from http://github.com/reflex-frp/reflex-platform
  cabal2nixResult = src: nixpkgs.runCommand "cabal2nixResult" {
    buildCommand = ''
      cabal2nix file://"${src}" >"$out"
    '';
    buildInputs = with nixpkgs; [
      cabal2nix
    ];
    # Support unicode characters in cabal files
    ${if !nixpkgs.stdenv.isDarwin then "LOCALE_ARCHIVE" else null} = "${nixpkgs.glibcLocales}/lib/locale/locale-archive";
    ${if !nixpkgs.stdenv.isDarwin then "LC_ALL" else null} = "en_US.UTF-8";
  };

  snap-loader-dynamic       = myrun ../deps/servant-snap/deps/snap-loader-dynamic;
  snap                      = myrun ../deps/servant-snap/deps/snap;
  snap-core                 = myrun ../deps/servant-snap/deps/snap/deps/snap-core;
  snap-server               = myrun ../deps/servant-snap/deps/snap/deps/snap-server;
  io-streams                = myrun ../deps/servant-snap/deps/snap/deps/io-streams;
  io-streams-haproxy        = myrun ../deps/servant-snap/deps/snap/deps/io-streams-haproxy;
  heist                     = myrun ../deps/servant-snap/deps/snap/deps/heist;
  xmlhtml                   = myrun ../deps/servant-snap/deps/snap/deps/xmlhtml;
  snap-loader-static        = myrun ../deps/servant-snap/deps/snap-loader-static;
  snaplet-postgresql-simple = myrun ../deps/snaplet-postgresql-simple;
  
  servant-snap              = myrun ../deps/servant-snap;
  servant                   = myrun ../deps/servant-snap/deps/servant;
  servant-lucid             = myrun ../deps/servant-snap/deps/servant/servant-lucid;
  servant-blaze             = myrun ../deps/servant-snap/deps/servant/servant-blaze;
  servant-server            = myrun ../deps/servant-snap/deps/servant/servant-server;
  servant-client            = myrun ../deps/servant-snap/deps/servant/servant-client;
  servant-docs              = myrun ../deps/servant-snap/deps/servant/servant-docs;
  servant-foreign           = myrun ../deps/servant-snap/deps/servant/servant-foreign;
  servant-js                = myrun ../deps/servant-snap/deps/servant/servant-js;

  servant-matlab            = myrun ../deps/servant-matlab;
  groundhog-th              = myrun ../deps/groundhog/groundhog-th;
  tagging-common            = myrun ../tagging-common;

  #tagging-common           = import ../tagging-common/default.nix { compilername = "ghc"; };
  #tagging-common           = import ../tagging-common/default.nix { 
  # compilername = "ghc"; 
  # mkDerivation = pkgs.stdenv.mkDerivation;
  # inherit (pkgs) aeson base base64-bytestring bytestring deepseq;
  # inherit (pkgs) errors filepath groundhog groundhog-postgresql groundhog-th;
  # inherit (pkgs) lens monad-logger resource-pool servant servant-docs stdenv;
  # inherit (pkgs) text time transformers uuid vector;
  #};


  f = { mkDerivation, aeson, base, bytestring, clock, configurator
      , containers, directory, errors, filepath, groundhog
      , groundhog-postgresql, groundhog-th, http-types, lens
      , map-syntax, monad-logger, MonadCatchIO-transformers, mtl
      , optparse-applicative, postgresql-simple, resource-pool, s3-signer, servant-matlab
      , SHA, snap-loader-dynamic, snaplet-postgresql-simple
      , stdenv, string-qq, tagging-common
      , text, time, transformers, unordered-containers, uuid, wreq
      }:
      with snap-loader-dynamic;
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
          aeson base bytestring clock configurator containers directory
          errors filepath groundhog groundhog-postgresql groundhog-th heist
          http-types lens map-syntax monad-logger MonadCatchIO-transformers
          mtl optparse-applicative postgresql-simple resource-pool s3-signer
          servant servant-docs servant-foreign servant-js servant-matlab
          servant-snap snap snap-core snap-loader-dynamic snap-loader-static
          snap-server snaplet-postgresql-simple string-qq tagging-common text
          time transformers unordered-containers uuid wreq
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f { 
    inherit snap-loader-dynamic;
    inherit servant-matlab;
    inherit tagging-common;
    inherit snaplet-postgresql-simple;
    inherit groundhog-th;
  };

in

  if pkgs.lib.inNixShell then drv.env else drv

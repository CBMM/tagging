let

  dontCheck = (import <nixpkgs> {}).pkgs.haskell.lib.dontCheck;

  onlyGhc   = platform: pkg: if (platform == "ghc")   then [pkg] else [];
  onlyGhcjs = platform: pkg: if (platform == "ghcjs") then [pkg] else [];
  #onlyGhc   = platform: pkg: if (platform == "ghc")   then {pkg=pkg;} else {};
  #onlyGhcjs = platform: pkg: if (platform == "ghcjs") then {pkg=pkg;} else {};

  nixpkgs = (import <nixpkgs> {});

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
  } "";

  inherit ((import <nixpkgs> {}).pkgs.haskellPackages.override {
    overrides = self: super: builtins.listToAttrs (map (name: {
      inherit name;
      value = self.callPackage (./. + "/${name}") {};
    }) [ "tagging-common" "tagging-server" "tagging-client" ])
    // 
    {servant-snap = dontCheck (self.callPackage deps/servant-snap {});
     servant-matlab = dontCheck (self.callPackage deps/servant-matlab {});
     servant-foreign = dontCheck (self.callPackage deps/servant-snap/deps/servant/servant-foreign {});
     servant-js = dontCheck (self.callPackage deps/servant-snap/deps/servant/servant-js {});
     servant-docs = dontCheck (self.callPackage deps/servant-snap/deps/servant/servant-docs {});
     servant-client = dontCheck (self.callPackage deps/servant-snap/deps/servant/servant-client {});
     servant-blaze = dontCheck (self.callPackage (cabal2nixResult deps/servant-snap/deps/servant/servant-blaze) {});
     servant-lucid = dontCheck (self.callPackage deps/servant-snap/deps/servant/servant-lucid {});
     servant-server = dontCheck (self.callPackage (cabal2nixResult deps/servant-snap/deps/servant/servant-server) {});
     servant = dontCheck (self.callPackage deps/servant-snap/deps/servant/servant {});
     hspec-snap = self.callPackage (cabal2nixResult deps/servant-snap/deps/hspec-snap) {};
     io-streams = dontCheck (self.callPackage deps/snap/deps/io-streams {});
     io-streams-haproxy = dontCheck (self.callPackage deps/snap/deps/io-streams-haproxy {});
     snap = self.callPackage deps/snap {};
     heist = self.callPackage deps/snap/deps/heist {};
     xmlhtml = self.callPackage deps/snap/deps/xmlhtml {};
     snap-core = self.callPackage deps/snap/deps/snap-core {};
     snap-server = self.callPackage deps/snap/deps/snap-server {};
     snap-loader-static = self.callPackage deps/snap-loader-static {};
     snap-loader-dynamic = self.callPackage deps/snap-loader-dynamic {};
     snaplet-postgresql-simple = self.callPackage deps/snaplet-postgresql-simple {};
     groundhog-th = self.callPackage deps/groundhog/groundhog-th {}; 
    };
  }) callPackage;
in platform: path: (callPackage path {} ).env

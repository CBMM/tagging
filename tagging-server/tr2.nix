{ try-reflex, ... }:
let

  dontCheck = (import <nixpkgs> {}).pkgs.haskell.lib.dontCheck;
  nixpkgs = (import <nixpkgs> {});

  myrun = pkg: dontCheck (pkgs.callPackage(cabal2nixResult pxg) {});

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

in
try-reflex.ghc.override {
  overrides = self: super: { 
     # inherit (nixpkgs.haskellPackages) lens clock lens-aeson wreq HUnit;
     tagging-common      = dontCheck (self.callPackage (cabal2nixResult ../tagging-common) { compilername = "ghc"; });
     servant-snap        = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap) {});
     servant-matlab      = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-matlab) {});
     servant-foreign     = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-foreign) {});
     servant-js          = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-js) {});
     servant-docs        = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-docs) {});
     servant-client      = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-client) {});
     servant-blaze       = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-blaze) {});
     servant-lucid       = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-lucid) {});
     servant-server      = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-server) {});
     servant             = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant) {});
     hspec-snap          = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/hspec-snap) {});
     io-streams          = dontCheck (self.callPackage (cabal2nixResult ../deps/snap/deps/io-streams) {});
     io-streams-haproxy  = dontCheck (self.callPackage (cabal2nixResult ../deps/snap/deps/io-streams-haproxy) {});
     snap                = dontCheck (self.callPackage (cabal2nixResult ../deps/snap) {});
     heist               = dontCheck (self.callPackage (cabal2nixResult ../deps/snap/deps/heist) {});
     xmlhtml             = dontCheck (self.callPackage (cabal2nixResult ../deps/snap/deps/xmlhtml) {});
     snap-core           = dontCheck (self.callPackage (cabal2nixResult ../deps/snap/deps/snap-core) {});
     snap-server         = dontCheck (self.callPackage (cabal2nixResult ../deps/snap/deps/snap-server) {});
     snap-loader-static  = dontCheck (self.callPackage (cabal2nixResult ../deps/snap-loader-static) {});
     snap-loader-dynamic = dontCheck (self.callPackage (cabal2nixResult ../deps/snap-loader-dynamic) {});
     groundhog-th        = dontCheck (self.callPackage (cabal2nixResult ../deps/groundhog/groundhog-th) {}); 
     snaplet-postgresql-simple = dontCheck (self.callPackage (cabal2nixResult ../deps/snaplet-postgresql-simple) {});
  };
}

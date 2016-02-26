{ reflex-platform, ... }:
let

  dontCheck = (import <nixpkgs> {}).pkgs.haskell.lib.dontCheck;

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
  };

in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
     tagging-common      = dontCheck (self.callPackage ../tagging-common/default.nix { compilername = "ghcjs"; });
     servant             = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant) {});
     servant-docs        = dontCheck (self.callPackage (cabal2nixResult ../deps/servant-snap/deps/servant/servant-docs) {});
     yaml-ghcjs          = dontCheck (self.callPackage (cabal2nixResult ../deps/yaml-ghcjs) {});
     groundhog-th        = dontCheck (self.callPackage (cabal2nixResult ../deps/groundhog/groundhog-th) { compilername="ghcjs"; });
     reflex-dom-contrib  = dontCheck (self.callPackage (cabal2nixResult ../deps/reflex-dom-contrib) {});
     groundhog-postgresql = null;
  };
}

{ reflex-platform, ... }:
let

  nixpkgs = (import <nixpkgs> {});

in
reflex-platform.ghc.override {
  overrides = self: super: { 
     reflex-dom-contrib  = (self.callPackage (reflex-platform.cabal2nixResult ../deps/reflex-dom-contrib) {});
     tagging-common      = (self.callPackage ../tagging-common/default.nix { compilername = "ghc"; });
     tagging-client      = (self.callPackage ../tagging-client/default.nix {});
     servant             = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/servant/servant) {});
     servant-docs        = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/servant/servant-docs) {});
     yaml-ghcjs          = null;
     groundhog-th        = (self.callPackage ../deps/groundhog/groundhog-th { compilername = "ghc"; });
     groundhog            = (self.callPackage (reflex-platform.cabal2nixResult ../deps/groundhog/groundhog) {});
     groundhog-postgresql = (self.callPackage (reflex-platform.cabal2nixResult ../deps/groundhog/groundhog-postgresql) {});
  };
}

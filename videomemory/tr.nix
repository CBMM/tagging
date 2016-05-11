{ reflex-platform, ... }:
let


  nixpkgs = (import <nixpkgs> {});

in
reflex-platform.ghcjs.override {
  overrides = self: super: { 
     reflex-dom-contrib  = (self.callPackage (reflex-platform.cabal2nixResult ../deps/reflex-dom-contrib) {});
     tagging-common      = (self.callPackage ../tagging-common/default.nix { compilername = "ghcjs"; });
     tagging-client      = (self.callPackage ../tagging-client/default.nix {});
     servant             = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/servant/servant) {});
     servant-docs        = (self.callPackage (reflex-platform.cabal2nixResult ../deps/servant-snap/deps/servant/servant-docs) {});
     yaml-ghcjs          = (self.callPackage (reflex-platform.cabal2nixResult ../deps/yaml-ghcjs) {});
     groundhog-th        = (self.callPackage ../deps/groundhog/groundhog-th { compilername = "ghcjs"; });
     groundhog-postgresql = null;
  };
}

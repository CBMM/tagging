let
  inherit ((import <nixpkgs> {}).pkgs.haskellPackages.override {
    overrides = self: super: builtins.listToAttrs (map (name: {
      inherit name;
      value = self.callPackage (./. + "/${name}") {};
    }) [ "tagging-common" "tagging-server" "tagging-client" ])
    // { servant-snap = self.callPackage deps/servant-snap { }; }
    // { servant-matlab = self.callPackage ../servant-matlab {}; }
    // { servant-foreign = self.callPackage ../servant-snap/deps/servant/servant-foreign {}; }
    // { servant-js = self.callPackage ../servant-snap/deps/servant/servant-js {}; }
    // { servant-docs = self.callPackage ../servant-snap/deps/servant/servant-docs {}; }
    // { servant-client = self.callPackage ../servant-snap/deps/servant/servant-client {}; }
    // { servant-blaze = self.callPackage ../servant-snap/deps/servant/servant-blaze {}; }
    // { servant-lucid = self.callPackage ../servant-snap/deps/servant/servant-lucid {}; }
    // { servant-server = self.callPackage ../servant-snap/deps/servant/servant-server {}; }
    // { servant-mock = self.callPackage ../servant-snap/deps/servant/servant-mock {}; }
    // { servant = self.callPackage ../servant-snap/deps/servant/servant {}; }
    // { hspec-snap = self.callPackage ../hspec-snap {}; }
    // { io-streams = self.callPackage ../snap/deps/io-streams {}; }
    // { io-streams-haproxy = self.callPackage ../snap/deps/io-streams-haproxy {}; }
    // { snap = self.callPackage ../snap {}; }
    // { heist = self.callPackage ../snap/deps/heist {}; }
    // { xmlhtml = self.callPackage ../snap/deps/xmlhtml {}; }
    // { snap-core = self.callPackage ../snap/deps/snap-core {}; }
    // { snap-server = self.callPackage ../snap/deps/snap-server {}; }
    // { snap-loader-static = self.callPackage ../tagging/deps/snap-loader-static {}; }
    // { snap-loader-dynamic = self.callPackage ../tagging/deps/snap-loader-dynamic {}; }
    // { snaplet-postgresql-simple = self.callPackage ../tagging/deps/snaplet-postgresql-simple {}; }
    // { groundhog = self.callPackage deps/groundhog/groundhog {}; }
    // { groundhog-th = self.callPackage deps/groundhog/groundhog-th {}; }
    // { groundhog-postgresql = self.callPackage deps/groundhog/groundhog-postgresql {}; }
       ;
  }) callPackage;
in path: (callPackage path {} ).env

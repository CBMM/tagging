let
  inherit ((import <nixpkgs> {}).pkgs.haskellPackages.override {
    overrides = self: super: builtins.listToAttrs (map (name: {
      inherit name;
      value = self.callPackage (./. + "/${name}") {};
    }) [ "tagging-common" "tagging-server" "tagging-client" ]);
  }) callPackage;
  servant-foreign = callPackage ../../servant/servant-foreign;
in path: (callPackage path {}).env


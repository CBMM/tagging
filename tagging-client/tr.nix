{ try-reflex, ... }: 
let nixpkgs = import "<nixpkgs>";
in
try-reflex.ghcjs.override {
  overrides = self: super: {
    reflex = self.callPackage ../deps/reflex {};
    reflex-dom = self.callPackage ../deps/reflex-dom {};
    reflex-dom-contrib = self.callPackage ../deps/reflex-dom-contrib {};
    tagging-common = self.callPackage ../tagging-common {};
    semigroups = nixpkgs.fetchgit {
      url: git://github.com/ekmett/semigroups;
      hash: "3f0caec983d74aacdd2460399488de758e957a16";
      sha256: "";
  };
}

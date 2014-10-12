let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      trinity = pkgs.haskellPackages.callPackage ./. {};
    };
  };

in
  pkgs.lib.overrideDerivation haskellPackages.trinity (attrs: {
    buildInputs = [ haskellPackages.cabalInstall ] ++ attrs.buildInputs;
  })

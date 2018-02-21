let
  bootstrap = import <nixpkgs> {};
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
  src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    inherit (nixpkgs) rev sha256;
  };
  pkgs = import src {
    config = {
      packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskellPackagesOld: rec {
            pandoc = haskellPackagesOld.pandoc_2_1_1;
            hakyll = pkgs.haskell.lib.dontCheck haskellPackagesOld.hakyll_4_11_0_0;
            pandoc-citeproc = haskellPackagesNew.callHackage "pandoc-citeproc" "0.13" {};
          };
        };
      };
    };
  };
in
pkgs.callPackage ./hakyllColEx.nix {}

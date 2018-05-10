{ pkgs ? import <nixpkgs> {}, extras ? import ../nix/extras.nix } :
  pkgs.haskellPackages.callCabal2nix "ColEx" (extras.gitignoreSource ./.) {}

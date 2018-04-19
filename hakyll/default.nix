{ pkgs ? import <nixpkgs> {}, extras ? import ../extras.nix } :
  pkgs.haskellPackages.callCabal2nix "ColEx" (extras.gitignoreSource ./.. + "/hakyll") {}

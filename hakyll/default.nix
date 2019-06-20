{ pkgs ? import <nixpkgs> {}, extras ? import ../nix/extras.nix // import ../nix/gitignore.nix { inherit (import <nixpkgs> {}) lib; } } :
  pkgs.haskellPackages.callCabal2nix "colex-hakyll" (extras.gitignoreSource ./.) {}

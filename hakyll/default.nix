{ extras ? import ../nix/extras.nix // import ../nix/gitignore.nix { inherit (import <nixpkgs> {}) lib; }, 
  pkgs ? extras.pinnedPkgs { specFile = ../nix/nixpkgs.json; opts = {}; }
} :
    pkgs.haskellPackages.callCabal2nix "colex-hakyll" (extras.gitignoreSource ./.) {}

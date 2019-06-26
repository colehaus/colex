{ extras ? import ../../../nix/extras.nix // import ../../../nix/gitignore.nix { inherit (import <nixpkgs> {}) lib; } } :
  extras.purescriptDevEnv {
    pkgs = extras.pinnedPkgs { specFile = ./nixpkgs.json; opts = {}; };
  }

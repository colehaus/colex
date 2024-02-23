{ extras ? import ../../../nix/extras.nix // import ../../../nix/gitignore.nix { inherit (import <nixpkgs> {}) lib; } } :
  # Note that we vendor purescript-graphviz so we can remove the dependency
  # on viz.js in bower.json which no longer works and is not necessary
  extras.callPurescript2nix {
    pkgs = extras.pinnedPkgs { specFile = ./nixpkgs.json; opts = {}; };
    name = "causal-graphs";
    src = extras.gitignoreSource ./.;
    executable = true;
    npm = true;
  }
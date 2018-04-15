let
  extras = import ./extras.nix;
  pkgs = extras.pinnedPkgs {
    specFile = ./nixpkgs.json;
    opts = { config = { packageOverrides = import ./package-overrides.nix; }; };
  };
in
pkgs.callPackage ./hakyllColEx.nix { inherit pkgs extras; }

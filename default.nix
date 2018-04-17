let
  extras = import ./extras.nix;
  pkgs = extras.pinnedPkgs {
    specFile = ./nixpkgs.json;
    opts = { config = { packageOverrides = import ./package-overrides.nix; }; };
  };
  hakyll = pkgs.callPackage ./hakyll { inherit pkgs; };
in
pkgs.callPackage ./content { inherit pkgs extras hakyll; }

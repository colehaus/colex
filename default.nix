let
  extras =
    import ./extras.nix //
    import ./gitignore.nix { inherit (pkgs) lib; };
  pkgs = extras.pinnedPkgs {
    specFile = ./nixpkgs.json;
    opts = { config = { packageOverrides = import ./package-overrides.nix; }; };
  };
  hakyll = pkgs.callPackage ./hakyll { inherit pkgs extras; };
in
pkgs.callPackage ./content { inherit pkgs extras hakyll; }

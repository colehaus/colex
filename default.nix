let
  extras =
    import ./nix/extras.nix //
    import ./nix/gitignore.nix { inherit (pkgs) lib; };
  pkgs = extras.pinnedPkgs {
    specFile = ./nix/nixpkgs.json;
    opts = { config = { packageOverrides = import ./nix/package-overrides.nix; }; };
  };
  hakyll = pkgs.callPackage ./hakyll { inherit pkgs extras; };
in
pkgs.callPackage ./content { inherit pkgs extras hakyll; }

let
  pkgs = import ./pinned-pkgs.nix ./nixpkgs.json { config = { packageOverrides = import ./package-overrides.nix; }; };
in
pkgs.callPackage ./hakyllColEx.nix {}

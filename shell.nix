let
  extras = import ./extras.nix;
  pkgs = extras.pinnedPkgs {
    specFile = ./nixpkgs.json;
    opts = { config = { packageOverrides = import ./package-overrides.nix; }; };
  };
  webpack = pkgs.callPackage ./webpackColEx.nix {};
  hakyll = pkgs.callPackage ./hakyllColEx.nix {};
in
  pkgs.stdenv.mkDerivation {
    name = "colExEnv";
    LC_ALL = "en_US.UTF-8";
    buildInputs =
      hakyll.nativeBuildInputs ++
      [
        pkgs.stack
        pkgs.nodejs
        pkgs.flow
        webpack.nodeDependencies
      ] ++
      (if pkgs.stdenv.isDarwin then [ pkgs.darwin.apple_sdk.frameworks.Cocoa ] else []) ++
      (if pkgs.stdenv.isLinux then [ pkgs.glibcLocales ] else []);
  }

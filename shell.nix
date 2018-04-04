let
  pkgs = import ./pinned-pkgs.nix ./nixpkgs.json { config = { packageOverrides = import ./package-overrides.nix; }; };
  webpack = pkgs.callPackage ./webpackColEx.nix {};
  hakyll = pkgs.callPackage ./hakyllColEx.nix {};
in
  pkgs.stdenv.mkDerivation rec {
    name = "colExEnv";
    LC_ALL = "en_US.UTF-8";
    buildInputs =
      hakyll.nativeBuildInputs ++
      [
        pkgs.stack
        pkgs.nodejs
        webpack.nodeDependencies
      ] ++
      (if pkgs.stdenv.isDarwin then [ pkgs.darwin.apple_sdk.frameworks.Cocoa ] else []) ++
      (if pkgs.stdenv.isLinux then [ pkgs.glibcLocales ] else []);
  }

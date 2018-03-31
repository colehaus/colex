let
  pkgs = import ./pinned-pkgs.nix ./nixpkgs.json {};
  webpack = pkgs.callPackage ./webpackColEx.nix {};
  npmDependencies = pkgs.callPackage ./callNpm2nix.nix {
    name = "npm-dependencies";
    npmPkgs = [
      { eslint = "^4.18.2"; }
      { mathjax-node-cli = "^1.0.0"; }
      { uglify-js = "^3.3.10"; }
      { standard = "^11.0.0"; }
    ];
  };
in
  pkgs.stdenv.mkDerivation rec {
    name = "colExEnv";
    LC_ALL = "en_US.UTF-8";
    buildInputs = [
      pkgs.git
      pkgs.nodejs

      pkgs.purescript
      pkgs.nodePackages.pulp

      pkgs.librsvg
      pkgs.sass
      npmDependencies."mathjax-node-cli-^1.0.0"
      npmDependencies."uglify-js-^3.3.10"

      npmDependencies."eslint-^4.18.2"
      npmDependencies."standard-^11.0.0"

      pkgs.stack
      pkgs.gcc
      pkgs.zlib
      pkgs.expat
      pkgs.libiconv

      pkgs.flow
      webpack.nodeDependencies
    ] ++
    (if pkgs.stdenv.isDarwin then [ pkgs.darwin.apple_sdk.frameworks.Cocoa ] else []) ++
    (if pkgs.stdenv.isLinux then [ pkgs.glibcLocales ] else []);
  }

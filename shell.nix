{ pkgs ? import <nixpkgs> {} } :
  let
    webpack = pkgs.callPackage ./webpackColEx.nix {};
    npmDependencies = pkgs.callPackage ./callNpm2nix.nix {
      name = "mathjax-node-cli";
      npmPkgs = [ { mathjax-node-cli = "^1.0.0"; } ];
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

        pkgs.sass
        npmDependencies."mathjax-node-cli-^1.0.0"

        pkgs.stack
        pkgs.gcc
        pkgs.zlib
        pkgs.expat
        pkgs.libiconv
        pkgs.darwin.apple_sdk.frameworks.Cocoa

        webpack.nodeDependencies
      ];
    }

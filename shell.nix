{ pkgs ? import <nixpkgs> {} } :
  let
    webpack = pkgs.callPackage ./webpackColEx.nix {};
  in
    pkgs.stdenv.mkDerivation rec {
      name = "colExEnv";
      buildInputs = [
        pkgs.git

        pkgs.purescript
        pkgs.nodePackages.pulp

        pkgs.stack
        pkgs.sass

        pkgs.nodejs
      ];
      # There's probably some way to get these properly on the PATH but I don't yet know it
      NODE_DEPENDENCIES = webpack.NODE_DEPENDENCIES;
      NODE_PATH = webpack.NODE_PATH;
    }

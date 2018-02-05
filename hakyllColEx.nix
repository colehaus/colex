{ pkgs ? import <nixpkgs> {}, webpackColEx } :
  let
    generator = pkgs.haskellPackages.callCabal2nix "ColEx" ./hakyll {};
  in
    pkgs.stdenv.mkDerivation {
      name = "hakyllColEx";
      src = ./content;
      phases = "unpackPhase buildPhase";
      nativeBuildInputs = [
        generator
        pkgs.sass
      ];
      NODE_DEPENDENCIES = webpackColEx.NODE_DEPENDENCIES;
      inherit webpackColEx;
      LC_ALL = "en_US.UTF-8";
      buildPhase = ''
        site clean
        rm -rf dist
        mkdir dist
        cp -r "$webpackColEx"/* dist
        site build
        mkdir $out
        cp -r _site/* $out
      '';
    }

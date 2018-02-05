{ pkgs ? import <nixpkgs> {}, webpackColEx } :
  let
    generator = pkgs.haskellPackages.callCabal2nix "ColEx" ./hakyll {};
    nodeEnv = pkgs.callPackage ./callNpm2nix.nix {
      name = "forHakyll";
      npmPkgs = [
        { uglify-js = "^3.3.9"; }
        { better-babel-cli = "^1.2.3"; }
      ];
    };
  in
    pkgs.stdenv.mkDerivation {
      name = "hakyllColEx";
      src = ./content;
      phases = "unpackPhase buildPhase";
      nativeBuildInputs = [
        generator
        pkgs.sass
        nodeEnv."better-babel-cli-^1.2.3"
        nodeEnv."uglify-js-^3.3.9"
        webpackColEx
      ];
      LC_ALL = "en_US.UTF-8";
      buildPhase = ''
        site clean
        site build
        mkdir $out
        cp -r _site/* $out
      '';
    }

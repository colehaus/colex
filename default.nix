{ pkgs ? import <nixpkgs> {} } :
  let
    generator = pkgs.haskellPackages.callCabal2nix "ColEx" ./hakyll {};
  in
    pkgs.stdenv.mkDerivation {
      name = "colExWebsite";
      src = ./content;
      phases = "unpackPhase buildPhase";
      buildInputs = with pkgs; [
        generator
        sass
        nodePackages_6_x.uglify-js
        nodePackages_6_x.better-babel-cli ];
      LC_ALL = "en_US.UTF-8";
      buildPhase = ''
        site build
        mkdir $out
        cp -r _site/* $out
      '';
    }

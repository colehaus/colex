let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
  generator = pkgs.haskellPackages.callCabal2nix "ColEx" ./infrastructure {};
in {
  colExWebsite =
    stdenv.mkDerivation {
      name = "colExWebsite";
      src = ./content;
      phases = "unpackPhase buildPhase";
      version = "0.1";
      buildInputs = with pkgs; with nodePackages_6_x; [ generator sass uglify-js better-babel-cli ];
      buildPhase = ''
        export LC_ALL=en_US.UTF-8
        site build
        mkdir $out
        cp -r _site/* $out
      '';
    };
}

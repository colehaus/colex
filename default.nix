let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
  generatorFn = { mkDerivation, hakyll, regex-compat-tdfa, pandoc, containers, filepath, stdenv }:
    mkDerivation {
      pname = "ColEx";
      version = "0.1.0.0";
      license = stdenv.lib.licenses.agpl3;
      src = ./infrastructure;
      isExecutable = true;
      isLibrary = false;
      doHaddock = false;
      executableHaskellDepends = [ hakyll regex-compat-tdfa pandoc containers filepath ];
    };
  generator = pkgs.haskellPackages.callPackage generatorFn {};
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

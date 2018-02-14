{ pkgs ? import <nixpkgs> { } } :
  let
    nodeEnv = pkgs.callPackage ./callNode2nix.nix {
      name = "webpackColEx";
      src = ./content/js/package.json;
    };
  in
    pkgs.stdenv.mkDerivation rec {
      name = "webpackColEx";
      src = ./content/js;
      phases = [ "unpackPhase" "buildPhase" ];
      nativeBuildInputs = [ pkgs.nodejs nodeEnv.shell.nodeDependencies ];
      nodeDependencies = nodeEnv.shell.nodeDependencies;
      NODE_ENV = "production";
      buildPhase = ''
        OUT_DIR="$out" webpack
      '';
    }

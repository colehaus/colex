{ pkgs ? import <nixpkgs> { } } :
  let
    nodeEnv = pkgs.callPackage ./callNode2nix.nix {
      name = "webpackColEx";
      src = ./content/js/package.json;
    };
  in
    pkgs.stdenv.mkDerivation {
      name = "webpackColEx";
      src = ./content/js;
      phases = [ "unpackPhase" "buildPhase" ];
      nativeBuildInputs = [
        pkgs.nodejs
        nodeEnv.shell.nodeDependencies
        pkgs.flow
      ];
      nodeDependencies = nodeEnv.shell.nodeDependencies;
      NODE_ENV = "production";
      buildPhase = ''
        OUT_DIR="$out" webpack
      '';
    }

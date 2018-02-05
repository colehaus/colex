{ pkgs ? import <nixpkgs> { } } :
  let
    nodeEnv = pkgs.callPackage ./callNode2nix.nix {
      name = "ColEx";
      src = ./content/package.json;
    };
  in
    pkgs.stdenv.mkDerivation rec {
      name = "webpackColEx";
      src = ./content;
      phases = "unpackPhase buildPhase";
      nativeBuildInputs = [ pkgs.nodejs nodeEnv.shell ];
      NODE_DEPENDENCIES = nodeEnv.shell.nodeDependencies;
      NODE_PATH = "${NODE_DEPENDENCIES}/lib/node_modules";
      NODE_ENV = "production";
      buildPhase = ''
        OUT_DIR="$out" "$NODE_DEPENDENCIES"/bin/webpack
      '';
    }

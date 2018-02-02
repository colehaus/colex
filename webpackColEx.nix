{ pkgs ? import <nixpkgs> { } } :
  let
    nodeEnv = pkgs.callPackage ./callNode2nix.nix {
      name = "ColEx";
      src = ./content;
    };
  in
    pkgs.stdenv.mkDerivation rec {
      name = "webpackColEx";
      src = ./content;
      phases = "unpackPhase buildPhase";
      buildInputs = [ pkgs.nodejs nodeEnv.shell ];
      nodeDependencies = nodeEnv.shell.nodeDependencies;
      NODE_PATH = "${nodeDependencies}/lib/node_modules";
      buildPhase = ''
        "$nodeDependencies"/bin/webpack --env.outDir="$out"
      '';
    }

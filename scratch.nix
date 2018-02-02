{ pkgs ? import <nixpkgs> { } } :
  let
    callNode2nix = import ./callNode2nix.nix;
    nodeEnv = callNode2nix { inherit pkgs; } "ColEx" ./content;
  in
    pkgs.stdenv.mkDerivation rec {
      name = "colExWebpack";
      srcs = [ ./content ];
      phases = "unpackPhase buildPhase";
      buildInputs = [ pkgs.nodejs nodeEnv.shell ];
      nodeDependencies = nodeEnv.shell.nodeDependencies;
      NODE_PATH = "${nodeDependencies}/lib/node_modules";
      buildPhase = ''
        "$nodeDependencies"/bin/webpack --env.outDir="$out"
      '';
    }

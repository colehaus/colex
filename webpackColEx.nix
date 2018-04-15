{ pkgs ? import <nixpkgs> {}, extras ? import ./extras.nix } :
  let
    nodeEnv = extras.callNode2nix {
      inherit pkgs;
      name = "webpackColEx";
      src = ./content/js/package.json;
      # TODO: Figure out a less hacky way to do this
      # (The root issue here is that node2nix has flattened the dependencies badly.)
      postBuild = ''
        sed --in-place s/sources.\"resolve-from-1.0.1\"/sources.\"resolve-from-3.0.0\"/ node-packages.nix
      '';
    };
  in
    pkgs.stdenv.mkDerivation {
      name = "webpackColEx";
      src = ./content/js;
      phases = [ "unpackPhase" "buildPhase" ];
      nativeBuildInputs = [
        pkgs.nodejs
        pkgs.flow
        nodeEnv.shell.nodeDependencies
      ];
      nodeDependencies = nodeEnv.shell.nodeDependencies;
      NODE_ENV = "production";
      buildPhase = ''
        OUT_DIR="$out" webpack
      '';
    }

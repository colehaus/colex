{ pkgs ? import <nixpkgs> {}, extras ? import ../../nix/extras.nix } :
  let
    nodeEnv = extras.callNode2nix {
      inherit pkgs;
      name = "webpackColEx";
      src = ./package.json;
    };
  in
    pkgs.stdenv.mkDerivation rec {
      name = "webpackColEx";
      phases = [ "unpackPhase" "buildPhase" ];
      nativeBuildInputs = [
        pkgs.nodejs
        pkgs.flow
        nodeEnv.shell.nodeDependencies
      ];
      src = extras.gitignoreSource ./.;
      NODE_ENV = "production";
      buildPhase = ''
        # Default `NODE_PATH` doesn't work with scoped packages
        for path in ''${NODE_PATH//:/ }; do
            if [[ "$path" = *"node-dependencies-ColEx"* ]]; then
                NODE_PATH="$path/@colehaus":$NODE_PATH
                NODE_PATH="$path/@babel":$NODE_PATH
            fi
        done
        # Webpack doesn't read NODE_PATH so this seems like the most convenient way to tell about it required deps
        rm -r node_modules
        ln -s ${nodeEnv.shell.nodeDependencies}/lib/node_modules node_modules
        OUT_DIR="$out" webpack
      '';
    }

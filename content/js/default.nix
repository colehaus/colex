{ pkgs ? import <nixpkgs> {}, extras ? import ../../nix/extras.nix // import ../../nix/gitignore.nix { inherit (import <nixpkgs> {}) lib; } } :
  let
    nodeEnv = extras.callNode2nix {
      inherit pkgs;
      name = "webpackColEx";
      package = ./package.json;
      package-lock = ./package-lock.json;
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
        # e.g. `webpack-cli` expects `flow-webpack-plugin` to be a direct child of one of the `NODE_PATH` entries
        for path in ''${NODE_PATH//:/ }; do
            if [[ "$path" = *"node-dependencies-ColEx"* ]]; then
                NODE_PATH="$path/@colehaus":$NODE_PATH
            fi
        done
        # babel doesn't read NODE_PATH so this seems like the most convenient way to tell it about required deps
        mkdir node_modules
        ln -s ${nodeEnv.shell.nodeDependencies}/lib/node_modules/@babel node_modules/@babel
        OUT_DIR="$out" webpack --display-error-details
      '';
    }

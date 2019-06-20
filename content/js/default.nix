{ extras ? import ../../nix/extras.nix // import ../../nix/gitignore.nix { inherit (import <nixpkgs> {}) lib; }, pkgs ? extras.pinnedPkgs { specFile = ../../nix/nixpkgs.json; opts = {}; } } :
  let
    nodeNix = extras.callNode2nix {
      inherit pkgs;
      name = "colex-webpack";
      package = ./package.json;
      packageLock = ./package-lock.json;
      postBuild = ''
        sed -i -e 's/dontNpmInstall ? false/dontNpmInstall ? true/g' node-env.nix
      '';
    };
    nodeEnv = pkgs.callPackage nodeNix {};
  in
    pkgs.stdenv.mkDerivation rec {
      name = "colex-webpack";
      phases = [ "unpackPhase" "configurePhase" "buildPhase" ];
      nativeBuildInputs = [
        pkgs.nodejs
        # `flow-bin` from npm doesn't work with nix
        pkgs.flow
        nodeNix
        nodeEnv.shell.nodeDependencies
      ];
      src = extras.gitignoreSource ./.;
      NODE_ENV = "production";
      # Default `NODE_PATH` doesn't work with scoped packages
      # e.g. `webpack-cli` expects `flow-webpack-plugin` to be a direct child of one of the `NODE_PATH` entries
      nodePathFix = ''
        for path in ''${NODE_PATH//:/ }; do
            if [[ "$path" = *"node-dependencies-ColEx"* ]]; then
                NODE_PATH="$path/@colehaus":$NODE_PATH
            fi
        done
      '';
      # babel doesn't read NODE_PATH so this seems like the most convenient way to tell it about required deps
      babelDepFix = ''
        rm -rf node_modules
        mkdir node_modules
        ln -s ${nodeEnv.shell.nodeDependencies}/lib/node_modules/@babel node_modules/@babel
      '';
      configurePhase = ''
        ${nodePathFix}
        ${babelDepFix}
      '';
      buildPhase = ''
        OUT_DIR="$out" webpack --display-error-details
      '';
      shellHook = configurePhase;
    }

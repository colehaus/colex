{ pkgs ? import <nixpkgs> {}, name, npmPkgs } :
  let
    npm2nix = pkgs.stdenv.mkDerivation {
      inherit name;
      nativeBuildInputs = [ pkgs.nodePackages.node2nix pkgs.nix ];
      phases = [ "installPhase" ];
      npmPkgsJson = builtins.toJSON npmPkgs;
      installPhase = ''
        mkdir -p "$out"
        echo "$npmPkgsJson" > node-packages.json
        node2nix --input node-packages.json
        cp *.nix "$out"
      '';
    };
  in
    pkgs.callPackage npm2nix {}

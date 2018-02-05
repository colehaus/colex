{ pkgs ? import <nixpkgs> {}, name, npmPkgs } :
  let
    npm2nix = pkgs.runCommand name {
      nativeBuildInputs = [ pkgs.nodePackages.node2nix pkgs.nix ];
      npmPkgsJson = builtins.toJSON npmPkgs;
    } ''
        mkdir -p "$out"
        echo "$npmPkgsJson" > node-packages.json
        node2nix --input node-packages.json
        cp *.nix "$out"
    '';
  in
    pkgs.callPackage npm2nix {}

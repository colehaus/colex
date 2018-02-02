{ pkgs ? import <nixpkgs> {}, name, src } :
  let
    node2nix = pkgs.stdenv.mkDerivation {
      name = "node2nix-${name}";
      nativeBuildInputs = [ pkgs.nodePackages.node2nix pkgs.nix ];
      preferLocalBuild = true;
      phases = [ "installPhase" ];
      inherit src;
      installPhase = ''
        mkdir -p "$out"
        cd "$out"
        node2nix --development -6 --input "$src"/package.json
        # Fix up relative path for compatibility with nix
        # TODO: Fix this unseemly hack (and the fact that we take a dir)
        sed -i 's|src = \.\.|src = /nix/store|' node-packages.nix
      '';
      };
  in
    pkgs.callPackage node2nix {}

{ pkgs ? import <nixpkgs> {}, name, npmPkgs } :
  let
    npm2nix = pkgs.stdenv.mkDerivation {
      name = "npm2nix-${name}";
      nativeBuildInputs = [ pkgs.nodePackages.node2nix pkgs.nix ];
      phases = [ "unpackPhase" "buildPhase" "installPhase" ];
      src = pkgs.writeTextDir "node-packages.json" (builtins.toJSON npmPkgs);
      buildPhase = ''
        node2nix -6 --input node-packages.json
      '';
      installPhase = ''
        mkdir -p "$out"
        cp *.nix "$out"
      '';
    };
  in
    pkgs.callPackage npm2nix {}

{ pkgs ? import <nixpkgs> {}, name, src } :
  let
    packageJson = pkgs.writeTextDir "package.json" (builtins.readFile src);
    node2nix = pkgs.stdenv.mkDerivation {
      name = "node2nix-${name}";
      nativeBuildInputs = [ pkgs.nodePackages.node2nix pkgs.nix ];
      phases = [ "buildPhase" "installPhase" ];
      src = packageJson;
      preferLocalBuild = true;
      buildPhase = ''
        node2nix --development -6 --input "$src"/package.json
      '';
      installPhase = ''
        mkdir -p "$out"
        cp *.nix "$out"
      '';
    };
  in
    pkgs.callPackage node2nix {}

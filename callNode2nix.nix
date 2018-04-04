{ pkgs ? import <nixpkgs> {}, name, src, postBuild ? "" } :
  let
    packageJson = pkgs.writeTextDir "package.json" (builtins.readFile src);
    node2nix = pkgs.stdenv.mkDerivation {
      name = "node2nix-${name}";
      nativeBuildInputs = [ pkgs.nodePackages.node2nix pkgs.nix ];
      phases = [ "buildPhase" "installPhase" ];
      src = packageJson;
      preferLocalBuild = true;
      # We need the temporary directory so that `node2nix`'s relative paths are correct
      buildPhase = ''
        TMP=$(mktemp -d)
        cd $TMP
        node2nix --development -6 --input "$src"/package.json
        ${postBuild}
      '';
      installPhase = ''
        mkdir -p "$out"
        cp *.nix "$out"
      '';
    };
  in
    pkgs.callPackage node2nix {}

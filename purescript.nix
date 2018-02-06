{ pkgs ? import <nixpkgs> { }, name, src } :
  let
    bowerDeps = pkgs.callPackage ./callBower2nix.nix {
      name = "purescriptDeps-${name}";
      src = "${src}/bower.json";
    };
  in
    pkgs.stdenv.mkDerivation {
      inherit src;
      inherit name;
      inherit bowerDeps;
      nativeBuildInputs = [
        pkgs.nodePackages.pulp
        pkgs.purescript
      ];
      phases = [ "unpackPhase" "configurePhase" "buildPhase"];
      configurePhase = ''
        mkdir -p bower_components
        for hash in "$bowerDeps"/packages/*; do
          for version in "$hash"/*; do
            echo $hash
            echo $version
            cp -r "$version" bower_components/purescript-$(basename "$hash")
          done
        done
      '';
      buildPhase = ''
        pulp browserify --optimise --to "$out"/"$name".js
      '';
    }

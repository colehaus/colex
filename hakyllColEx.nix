{ pkgs ? import <nixpkgs> {} } :
  let
    generator = pkgs.haskellPackages.callCabal2nix "ColEx" ./hakyll {};
    webpackColEx = pkgs.callPackage ./webpackColEx.nix {};
    bibliometric = pkgs.callPackage ./purescript.nix {
      name = "bibliometric";
      src = ./content/js/bibliometric;
    };
  in
    pkgs.stdenv.mkDerivation {
      name = "hakyllColEx";
      src = ./content;
      phases = [ "unpackPhase" "patchPhase" "buildPhase" "installPhase" ];
      nativeBuildInputs = [
        generator
        pkgs.sass
      ];
      inherit webpackColEx;
      inherit bibliometric;
      LC_ALL = "en_US.UTF-8";
      # TODO Remove `rm` once we reorganize source directory
      patchPhase = ''
        rm -rf dist
        mkdir dist
        cp "$webpackColEx"/* dist
        cp "$bibliometric"/* dist
      '';
      buildPhase = ''
        site rebuild
      '';
      installPhase = ''
        mkdir "$out"
        cp -r _site/* $out
      '';
    }

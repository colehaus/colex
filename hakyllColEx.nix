{ pkgs ? import <nixpkgs> {} } :
  let
    hakyll = pkgs.haskellPackages.callCabal2nix "ColEx" ./hakyll {};
    webpackColEx = pkgs.callPackage ./webpackColEx.nix {};
    bibliometric = pkgs.callPackage ./purescript.nix {
      name = "bibliometric";
      src = ./content/js/bibliometric;
    };
    npmDependencies = pkgs.callPackage ./callNpm2nix.nix {
      name = "mathjax-node-cli";
      npmPkgs = [ { mathjax-node-cli = "^1.0.0"; } ];
    };
  in
    pkgs.stdenv.mkDerivation {
      name = "hakyllColEx";
      src = ./content;
      phases = [ "unpackPhase" "patchPhase" "buildPhase" "installPhase" ];
      nativeBuildInputs = [
        hakyll
        pkgs.sass
        npmDependencies."mathjax-node-cli-^1.0.0"
      ];
      inherit webpackColEx;
      inherit bibliometric;
      LC_ALL = "en_US.UTF-8";
      WEB_HOST = "https://colehaus.github.io/ColEx";
      MATH_RENDER_METHOD = "MathjaxNode";
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

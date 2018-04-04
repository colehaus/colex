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
      npmPkgs = [
        { mathjax-node-cli = "^1.0.0"; }
        { uglify-js = "^3.3.10"; }
      ];
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
        npmDependencies."uglify-js-^3.3.10"
      ] ++ (if pkgs.stdenv.isLinux then [ pkgs.glibcLocales ] else []);
      inherit webpackColEx;
      inherit bibliometric;
      LC_ALL = "en_US.UTF-8";
      WEB_HOST = "https://www.col-ex.org";
      MATH_RENDER_METHOD = "FeedlyCompatible";
      # TODO Remove `rm` once we reorganize source directory
      patchPhase = ''
        rm -rf js/dist
        mkdir js/dist
        cp "$webpackColEx"/* js/dist
        cp "$bibliometric"/* js/dist
      '';
      buildPhase = ''
        site rebuild
      '';
      installPhase = ''
        mkdir "$out"
        cp -r _site/* $out
      '';
    }

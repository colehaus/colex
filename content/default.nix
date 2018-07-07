{ pkgs ? import <nixpkgs> {}
, extras ? import ../nix/extras.nix
, hakyll ? pkgs.callPackage ../hakyll { inherit pkgs; }} :
  let
    webpackColEx = pkgs.callPackage ./js { inherit pkgs extras; };
    bibliometric = pkgs.callPackage ./js/bibliometric { inherit pkgs extras; };
    voi = pkgs.callPackage ./js/value-of-information-calculator { inherit pkgs extras; };
    vnm = pkgs.callPackage ./js/construct-vnm-utility-function { inherit pkgs extras; };
    exemplar = pkgs.callPackage ./js/exemplars-curse { inherit pkgs extras; };
    mathJaxNodeCli = extras.callNpm {
      inherit pkgs;
      name = "mathjax-node-cli";
      versionSpec = "^1.0.0";
    };
    uglifyJs = extras.callNpm {
      inherit pkgs;
      name = "uglify-js";
      versionSpec = "^3.3.10";
    };
  in
    pkgs.stdenv.mkDerivation rec {
      name = "hakyllColEx";
      src = extras.gitignoreSource ./.;
      phases = [ "unpackPhase" "patchPhase" "buildPhase" "installPhase" ];
      nativeBuildInputs = [
        hakyll
        pkgs.sass
        mathJaxNodeCli
        uglifyJs
      ] ++ (if pkgs.stdenv.isLinux then [ pkgs.glibcLocales ] else []);
      LC_ALL = "en_US.UTF-8";
      WEB_HOST = "https://www.col-ex.org";
      MATH_RENDER_METHOD = "FeedlyCompatible";
      # TODO Remove `rm` once we reorganize source directory
      patchPhase = ''
        mkdir js/dist
        cp ${webpackColEx}/* js/dist
        cp ${bibliometric}/* js/dist
        cp ${voi}/* js/dist
        cp ${vnm}/* js/dist
        cp ${exemplar}/* js/dist
      '';
      buildPhase = ''
        site build
      '';
      installPhase = ''
        mkdir "$out"
        cp -r _site/* $out
      '';
    }

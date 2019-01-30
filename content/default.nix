{ extras ? import ../nix/extras.nix // import ../nix/gitignore.nix { inherit (import <nixpkgs> {}) lib; }
, pkgs ? extras.pinnedPkgs { specFile = ../nix/nixpkgs.json; opts = {}; }
, hakyll ? pkgs.callPackage ../hakyll { inherit pkgs; }} :
  let
    webpackColEx = pkgs.callPackage ./js { inherit pkgs extras; };
    bibliometric = pkgs.callPackage ./js/bibliometric { inherit extras; };
    voi = pkgs.callPackage ./js/value-of-information-calculator { inherit extras; };
    vnm = pkgs.callPackage ./js/construct-vnm-utility-function { inherit extras; };
    exemplar = pkgs.callPackage ./js/exemplars-curse { inherit extras; };
    dominatedDecisions = pkgs.callPackage ./js/dominated-decisions { inherit extras; };
    priorityDecisions = pkgs.callPackage ./js/priority-decisions { inherit extras; };
    mathJaxNodeCli = extras.callNpm {
      inherit pkgs;
      name = "mathjax-node-cli";
      versionSpec = "^1.0.1";
    };
    uglifyJs = extras.callNpm {
      inherit pkgs;
      name = "uglify-js";
      versionSpec = "^3.4.9";
    };
    stylelint = extras.callNpm {
      inherit pkgs;
      name = "stylelint";
      versionSpec = "^9.9.0";
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
        stylelint
      ] ++ (if pkgs.stdenv.isLinux then [ pkgs.glibcLocales ] else []);
      LC_ALL = "en_US.UTF-8";
      WEB_HOST = "https://www.col-ex.org";
      MATH_RENDER_METHOD = "FeedlyCompatible";
      patchPhase = ''
        mkdir js/dist
        cp ${webpackColEx}/* js/dist
        cp ${bibliometric}/* js/dist
        cp ${voi}/* js/dist
        cp ${vnm}/* js/dist
        cp ${exemplar}/* js/dist
        cp ${dominatedDecisions}/* js/dist
        cp ${priorityDecisions}/* js/dist
      '';
      buildPhase = ''
        site build
      '';
      installPhase = ''
        mkdir "$out"
        cp -r _site/* $out
      '';
    }

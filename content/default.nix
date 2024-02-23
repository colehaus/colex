{ extras ? import ../nix/extras.nix // import ../nix/gitignore.nix { inherit (import <nixpkgs> {}) lib; }
, pkgs ? extras.pinnedPkgs { specFile = ../nix/nixpkgs.json; opts = {}; }
, hakyllColEx ? pkgs.callPackage ../hakyll { inherit pkgs; }} :
  let
    webpackColEx = pkgs.callPackage ./js { inherit pkgs extras; };
    bibliometric = pkgs.callPackage ./js/bibliometric { inherit extras; };
    voi = pkgs.callPackage ./js/value-of-information-calculator { inherit extras; };
    vnm = pkgs.callPackage ./js/construct-vnm-utility-function { inherit extras; };
    exemplar = pkgs.callPackage ./js/exemplars-curse { inherit extras; };
    decisionDemos = pkgs.callPackage ./js/decision-demos { inherit extras; };
    causalGraph = pkgs.callPackage ./js/causal-graphs {inherit extras; };
    mathJaxNodeCliNix = extras.callNode2nix {
      name = "mathjax-node-cli";
      package = ./mathjax-node-cli-package.json;
      packageLock = ./mathjax-node-cli-package-lock.json;
    };
    mathJaxNodeCli = pkgs.callPackage mathJaxNodeCliNix {};
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
    depsOf = list: pkgs.lib.lists.unique (builtins.filter pkgs.lib.attrsets.isDerivation (pkgs.lib.lists.concatMap (x: x.nativeBuildInputs ++ x.buildInputs) list ++ list));
  in
    pkgs.stdenv.mkDerivation rec {
      name = "colex-content";
      src = extras.gitignoreSource ./.;
      phases = [ "unpackPhase" "patchPhase" "buildPhase" "installPhase" ];
      noGcDeps = depsOf [ webpackColEx bibliometric voi vnm exemplar decisionDemos causalGraph ];
      nativeBuildInputs = [
        hakyllColEx
        pkgs.sass
        mathJaxNodeCli.nodeDependencies
        uglifyJs
        stylelint

        webpackColEx
        bibliometric
        voi
        vnm
        exemplar
        decisionDemos
        causalGraph
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
        cp ${decisionDemos}/* js/dist
        cp ${causalGraph}/* js/dist
      '';
      buildPhase = ''
        site build
      '';
      installPhase = ''
        mkdir "$out"
        cp -r _site/* $out
      '';
    }

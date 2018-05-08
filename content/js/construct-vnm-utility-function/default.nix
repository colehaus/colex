{ pkgs ? import <nixpkgs> {}, extras ? import ../../../extras.nix } :
  extras.callPurescript2nix {
    inherit pkgs;
    name = "construct-vnm-utility-function";
    src = extras.gitignoreSource ../../.. + "/content/js/construct-vnm-utility-function";
    executable = true;
    npm = true;
  }

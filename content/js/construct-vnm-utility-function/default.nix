{ pkgs ? import <nixpkgs> {}, extras ? import ../../../extras.nix } :
  extras.callPurescript2nix {
    inherit pkgs;
    name = "construct-vnm-utility-function";
    src = extras.gitignoreSource ./.;
    executable = true;
    npm = true;
  }

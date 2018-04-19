{ pkgs ? import <nixpkgs> {}, extras ? import ../../../extras.nix } :
  extras.callPurescript2nix {
    inherit pkgs;
    name = "bibliometric";
    src = extras.gitignoreSource ../../.. + "/content/js/bibliometric";
    executable = true;
  }

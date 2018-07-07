{ pkgs ? import <nixpkgs> {}, extras ? import ../../../nix/extras.nix } :
  extras.callPurescript2nix {
    inherit pkgs;
    name = "exemplars-curse";
    src = extras.gitignoreSource ./.;
    executable = true;
    npm = true;
  }

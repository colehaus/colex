{ pkgs ? import <nixpkgs> {}, extras ? import ../../../extras.nix } :
  extras.callPurescript2nix {
    inherit pkgs;
    name = "value-of-information-calculator";
    src = extras.gitignoreSource ../../.. + "/content/js/value-of-information-calculator";
    executable = true;
    npm = true;
  }

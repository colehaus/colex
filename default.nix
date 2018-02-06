{ pkgs ? import <nixpkgs> {} } :
  pkgs.callPackage ./hakyllColEx.nix {}

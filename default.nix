{ pkgs ? import <nixpkgs> {} } :
  pkgs.callPackage ./hakyllColEx.nix {
    webpackColEx = pkgs.callPackage ./webpackColEx.nix {};
  }

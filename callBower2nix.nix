{ pkgs ? import <nixpkgs> {}, name, src, stdenv } :
  let
    bowerJson = pkgs.writeTextDir "bower.json" (builtins.readFile src);
    bower2nix = pkgs.runCommand "bower2nix-${name}" {
      nativeBuildInputs = [ pkgs.nodePackages.bower2nix pkgs.nix ];
      GIT_SSL_CAINFO =
        if stdenv.isLinux
        then /etc/ssl/certs/ca-certificates.crt
        else /nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt;
      src = bowerJson;
      preferLocalBuild = true;
    } ''
      mkdir -p "$out"
      bower2nix "$src"/bower.json "$out"/default.nix
    '';
  in
    pkgs.callPackage bower2nix {}

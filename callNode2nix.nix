{ pkgs ? import <nixpkgs> {}, name, src } :
  let
    packageJson = pkgs.writeTextFile {
      name = "packageJson-${name}";
      text = builtins.readFile src;
      destination = "/package.json";
    };
    node2nix = pkgs.runCommand "node2nix-${name}" {
      nativeBuildInputs = [ pkgs.nodePackages.node2nix pkgs.nix ];
      src = packageJson;
      preferLocalBuild = true;
    } ''
      mkdir -p "$out"
      cd "$out"
      node2nix --development -6 --input "$src"/package.json
    '';
  in
    pkgs.callPackage node2nix {}

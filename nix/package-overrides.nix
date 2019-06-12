pkgs:
  let
    extras = import ./extras.nix;
    pkgsOld = extras.pinnedPkgs { specFile = ./nixpkgs-old.json; opts = {}; };
  in
{
  purescript = pkgsOld.purescript;
}

let
  extras =
    import ./extras.nix //
    import ./gitignore.nix { inherit (pkgs) lib; };
  pkgs = extras.pinnedPkgs {
    specFile = ./nixpkgs.json;
    opts = { config = { packageOverrides = import ./package-overrides.nix; }; };
  };
  webpack = pkgs.callPackage ./content/js { inherit pkgs extras; };
  content = pkgs.callPackage ./content { inherit hakyll pkgs extras; };
  hakyll = pkgs.callPackage ./hakyll { inherit pkgs extras; };
in
  pkgs.stdenv.mkDerivation {
    name = "colExEnv";
    LC_ALL = "en_US.UTF-8";
    buildInputs =
      hakyll.nativeBuildInputs ++
      content.nativeBuildInputs ++
      webpack.nativeBuildInputs ++
      (extras.purescriptDevEnv { inherit pkgs; }).buildInputs ++
      (if pkgs.stdenv.isDarwin then [ pkgs.darwin.apple_sdk.frameworks.Cocoa ] else []) ++
      (if pkgs.stdenv.isLinux then [ pkgs.glibcLocales ] else []);
  }

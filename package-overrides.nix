pkgs:
  rec {
    haskellPackages = pkgs.haskellPackages.override {
      overrides = haskellPackagesNew: haskellPackagesOld: rec {
        pandoc = haskellPackagesOld.pandoc_2_1_1;
        hakyll = pkgs.haskell.lib.dontCheck haskellPackagesOld.hakyll_4_11_0_0;
        pandoc-citeproc = haskellPackagesNew.callHackage "pandoc-citeproc" "0.13" {};
      };
    };
  }

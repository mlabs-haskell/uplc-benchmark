{
  perSystem = { config, self', ... }:
    let
      uplc-benchmark-types-plutarch-lib = config.mkLbHaskellPackage {
        name = "uplc-benchmark-plutarch-lib";
        src = ./.;
        files = [ "NftMarketplace.lbf" ];
        exposedModules = [ "LambdaBuffers.NftMarketplace.Plutarch" ];
        cabalBuildDepends = [
          "base"
          "lbr-plutarch"
          "plutarch"
          "plutus-core < 1.4"
          "lbf-prelude-plutarch"
          "lbf-plutus-plutarch"
        ];
        lbfGen = self'.packages.lbf-plutus-to-plutarch;
      };

      uplc-benchmark-types-plutarch =
        config.libHaskell.mkPackage (config.libPlutarch.mkPackage {
          name = "uplc-benchmark-plutarch";
          src = uplc-benchmark-types-plutarch-lib.outPath;
          ghcVersion = "ghc928";
          externalDependencies = [
            self'.packages.lbr-plutarch
            self'.packages.lbr-prelude
            self'.packages.lbf-prelude-plutarch
            self'.packages.lbf-plutus-plutarch
          ];
        });
    in
    {
      packages = {
        inherit uplc-benchmark-types-plutarch-lib;
        uplc-benchmark-types-plutarch =
          uplc-benchmark-types-plutarch.packages."uplc-benchmark-plutarch-lib:lib:uplc-benchmark-plutarch-lib";
      };
    };
}

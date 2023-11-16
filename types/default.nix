{
  perSystem = { config, self', ... }:
    let
      uplc-benchmark-plutarch-lib = config.mkLbHaskellPackage {
        name = "uplc-benchmark-plutarch-lib";
        src = ./.;
        files = [ "NftMarketplace.lbf" ];
        exposedModules = [ "LambdaBuffers.NftMarketplace.Plutarch" ];
        cabalBuildDepends = [ "base" "lbr-plutarch" "plutarch" ];
        lbfGen = self'.packages.lbf-plutus-to-plutarch;
      };
    in
    {
      packages = {
        inherit uplc-benchmark-plutarch-lib;
      };
    };
}

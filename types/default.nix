{
  perSystem = { config, ... }:
    let
      uplc-benchmark-types-plutarch = config.libLb.mkPlutarchPackage {
        name = "uplc-benchmark-types-plutarch";
        src = ./.;
        files = [ "NftMarketplace.lbf" ];
      };
    in
    {
      packages = {
        uplc-benchmark-types-plutarch-lib = uplc-benchmark-types-plutarch.raw;
        uplc-benchmark-types-plutarch-compiled =
          uplc-benchmark-types-plutarch.compiled.packages."uplc-benchmark-types-plutarch-lib:lib:uplc-benchmark-types-plutarch-lib";
      };
    };
}

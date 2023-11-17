{
  perSystem = { config, ... }:
    let
      uplc-benchmark-types-plutarch = config.libLb.mkPlutarchPackage {
        name = "uplc-benchmark-types-plutarch";
        src = ./.;
        files = [
          "NftMarketplace.lbf"
          "Dex.lbf"
        ];
      };
    in
    {
      packages = {
        uplc-benchmark-types-plutarch =
          uplc-benchmark-types-plutarch.packages."uplc-benchmark-types-plutarch-lib:lib:uplc-benchmark-types-plutarch-lib" // { inherit (uplc-benchmark-types-plutarch) passthru; };
      };
    };
}

{
  perSystem = { config, ... }:
    let
      uplc-benchmark-types-plutarch = config.libLb.mkLbPlutarchPackage {
        name = "uplc-benchmark-types-plutarch";
        src = ./.;
      };

      uplc-benchmark-types-plutus = config.libLb.mkLbPlutusPackage {
        name = "uplc-benchmark-types-plutus";
        src = ./.;
      };
    in
    {
      packages = {
        uplc-benchmark-types-plutarch =
          uplc-benchmark-types-plutarch.packages."uplc-benchmark-types-plutarch-lb:lib:uplc-benchmark-types-plutarch-lb";
        uplc-benchmark-types-plutus =
          uplc-benchmark-types-plutus.packages."uplc-benchmark-types-plutus-lb:lib:uplc-benchmark-types-plutus-lb";
      };
    };
}

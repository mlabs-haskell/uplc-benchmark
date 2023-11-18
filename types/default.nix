{
  perSystem = { config, ... }:
    let
      uplc-benchmark-types-plutarch = config.libLb.mkLbPlutarchPackage {
        name = "uplc-benchmark-types-plutarch";
        src = ./.;
      };
    in
    {
      packages = {
        uplc-benchmark-types-plutarch =
          uplc-benchmark-types-plutarch.packages."uplc-benchmark-types-plutarch-lb:lib:uplc-benchmark-types-plutarch-lb";
      };
    };
}

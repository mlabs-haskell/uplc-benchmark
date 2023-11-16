{ mkHaskellLib
, codegen-configs
, lbf-prelude
, lbf-plutus
}:

mkHaskellLib {
  inherit codegen-configs;

  imports = [ lbf-prelude lbf-plutus ];
  dependencies = [ "lbf-prelude-plutarch" "lbf-plutus-plutarch" ];
  classes = [ "Prelude.Eq" "Plutus.V1.PlutusData" ];
  configs = [ "plutarch-prelude.json" "plutarch-plutus.json" ];
}

{ callPackage
}:
let
  # TODO: Remove when merged upstream https://github.com/NixOS/nixpkgs/pull/291245
  python-secp256k1-cardano = callPackage ./packages/python-secp256k1-cardano { };
  frozenlist2 = callPackage ./packages/frozenlist2 { };
  graphlib-backport = callPackage ./packages/graphlib-backport { };
  uplc = callPackage ./packages/uplc { inherit frozenlist2 python-secp256k1-cardano; };
  pluthon = callPackage ./packages/pluthon { inherit graphlib-backport uplc; };
  opshin = callPackage ./packages/opshin { inherit uplc pluthon frozenlist2; };
in
{
  packages = {
    inherit opshin;
  };
}

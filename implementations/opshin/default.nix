{
  perSystem = { pkgs, config, ... }:
    let
      # TODO: Remove when merged upstream https://github.com/NixOS/nixpkgs/pull/291245
      python-secp256k1-cardano = pkgs.callPackage ./python-secp256k1-cardano { };
      frozenlist2 = pkgs.callPackage ./frozenlist2 { };
      graphlib-backport = pkgs.callPackage ./graphlib-backport { };
      uplc = pkgs.callPackage ./uplc { inherit frozenlist2 python-secp256k1-cardano; };
      pluthon = pkgs.callPackage ./pluthon { inherit graphlib-backport uplc; };
      opshin = pkgs.callPackage ./opshin { inherit uplc pluthon frozenlist2; };
    in
    {
      devShells.opshin-implementation = pkgs.mkShell {
        shellHook = config.pre-commit.installationScript;
        nativeBuildInputs = [
          opshin
        ];
      };

      packages.opshin-implementation-compiled = pkgs.stdenv.mkDerivation {
        name = "opshin-implementation-compiled";
        src = ./.;

        nativeBuildInputs = [ opshin ];

        enableParallelBuilding = true;
      };
    };
}

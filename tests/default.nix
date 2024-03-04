{ self, ... }:
{
  perSystem = { pkgs, self', config, ... }:
    let
      uplc-benchmark-tests = config.libPlutarch.mkPackage {
        name = "uplc-benchmark-tests";
        src = ./.;
        externalDependencies = [
          "${self.inputs.plutus-test}/plutus-context-builder"
          "${self.inputs.plutus-test}/plutus-unit"
          self'.packages.uplc-benchmark-types-plutus
        ];
      };

      binSources = {
        UPLC_BENCHMARK_BIN_PLUTARCH = self'.packages.plutarch-implementation-compiled.outPath;
        UPLC_BENCHMARK_BIN_AIKEN = self'.packages.aiken-implementation-compiled.outPath;
        UPLC_BENCHMARK_BIN_PLUTUS_TX = self'.packages.plutus-tx-implementation-compiled.outPath;
        UPLC_BENCHMARK_BIN_OPSHIN = self'.packages.opshin-implementation-compiled.outPath;
      };
    in
    {
      checks.uplc-benchmark = uplc-benchmark-tests.checks."uplc-benchmark:test:uplc-benchmark-tests".overrideAttrs (prev: {
        env = (prev.env or { }) // binSources;
      });

      devShells.tests = pkgs.mkShell {
        shellHook = config.pre-commit.installationScript;
        env = binSources;
        inputsFrom = [
          uplc-benchmark-tests.devShell
        ];
      };
    };
}

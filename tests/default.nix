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

      uplc-benchmark-test-runner = uplc-benchmark-tests.packages."uplc-benchmark:test:uplc-benchmark-tests";
    in
    {
      checks.uplc-benchmark = uplc-benchmark-tests.checks."uplc-benchmark:test:uplc-benchmark-tests".overrideAttrs (prev: {
        env = (prev.env or { }) // binSources;
      });

      packages = {
        data-files = pkgs.runCommand "data-files"
          {
            nativeBuildInputs = [ pkgs.gnuplot ];
            env = binSources;
          } ''
          ${uplc-benchmark-test-runner}/bin/uplc-benchmark-tests

          cp ${./script_size.plt} ./script_size.plt
          gnuplot script_size.plt
          mkdir $out
          cp *.png *.csv $out
        '';
      };

      devShells.tests = pkgs.mkShell {
        shellHook = config.pre-commit.installationScript;
        env = binSources;
        inputsFrom = [
          uplc-benchmark-tests.devShell
        ];
      };
    };
}

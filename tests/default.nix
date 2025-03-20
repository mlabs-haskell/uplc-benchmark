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
        ];
      };

      binSources = {
        UPLC_BENCHMARK_BIN_PLUTARCH = self'.packages.plutarch-implementation-compiled.outPath;
        UPLC_BENCHMARK_BIN_AIKEN = self'.packages.aiken-implementation-compiled.outPath;
        UPLC_BENCHMARK_BIN_PLUTUS_TX = self'.packages.plutus-tx-implementation-compiled.outPath;
        # UPLC_BENCHMARK_BIN_OPSHIN = self'.packages.opshin-implementation-compiled.outPath;
      };

      uplc-benchmark-data = uplc-benchmark-tests.packages."uplc-benchmark:exe:uplc-benchmark-data";

      # TODO: Remove when upstreamed to nixpkgs
      csv2md = pkgs.python3.pkgs.callPackage ./csv2md { };
    in
    {
      checks.uplc-benchmark = uplc-benchmark-tests.checks."uplc-benchmark:test:uplc-benchmark-tests".overrideAttrs (prev: {
        env = (prev.env or { }) // binSources;
      });

      packages = {
        data-files = pkgs.runCommand "data-files"
          {
            nativeBuildInputs = [ pkgs.gnuplot csv2md ];
            env = binSources;
          } ''
          ${uplc-benchmark-data}/bin/uplc-benchmark-data

          cp ${./budget_template.plt} ./budget_template.plt
          for f in budget_*.csv; do
            run=$(basename --suffix=.csv $f)
            cp budget_template.plt $run.plt
            substituteInPlace $run.plt \
              --replace-fail "RUN_NAME" $run
            gnuplot $run.plt
            rm $run.plt
          done

          cp ${./script_size.plt} ./script_size.plt
          gnuplot script_size.plt
          mkdir $out
          cp *.png *.csv $out
          for f in *.csv ; do
              csv2md $f > $out/$(basename --suffix=.csv $f).md
          done
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

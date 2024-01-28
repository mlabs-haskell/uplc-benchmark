{
  perSystem = { pkgs, self', config, ... }:
    let
      liqwid-libs = config.libUtils.applyPatches {
        name = "liqwid-libs-patched";
        src = pkgs.fetchFromGitHub {
          owner = "Liqwid-Labs";
          repo = "liqwid-libs";
          rev = "e45647e49a106d64799193491af4e52553917ead";
          hash = "sha256-ioUyXgpBGLYjLtMIFRFFtV+h8QoRIi3TaYsb2GgWrg4=";
        };
        patches = [ ./liqwid-libs-exports.patch ];
      };

      # TODO: Update from PR to master
      # https://github.com/mlabs-haskell/ply/pull/50
      ply = pkgs.fetchFromGitHub {
        owner = "mlabs-haskell";
        repo = "ply";
        rev = "0b58813ad022ea8fd35aa309b935ad5b474a7e1d";
        hash = "sha256-om9EEGrw09gm0i+i9GYp0KYWQMXAd6ZvAZPzW6cGSLw=";
      };

      uplc-benchmark-tests = config.libPlutarch.mkPackage {
        name = "uplc-benchmark-tests";
        src = ./.;
        externalDependencies = [
          "${liqwid-libs}/liqwid-plutarch-extra"
          "${liqwid-libs}/liqwid-script-export"
          "${liqwid-libs}/plutarch-benchmark"
          "${liqwid-libs}/plutarch-context-builder"
          "${liqwid-libs}/plutarch-quickcheck"
          "${liqwid-libs}/plutarch-unit"
          "${ply}/ply-core"
          "${ply}/ply-plutarch"
          self'.packages.uplc-benchmark-types-plutus
        ];
      };

      binSources = {
        UPLC_BENCHMARK_BIN_PLUTARCH = self'.packages.plutarch-implementation-compiled.outPath;
        UPLC_BENCHMARK_BIN_AIKEN = self'.packages.aiken-implementation-compiled.outPath;
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

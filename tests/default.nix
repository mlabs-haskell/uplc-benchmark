{
  perSystem = { pkgs, self', config, ... }:
    let
      liqwid-libs = pkgs.fetchFromGitHub {
        owner = "Liqwid-Labs";
        repo = "liqwid-libs";
        rev = "e45647e49a106d64799193491af4e52553917ead";
        hash = "sha256-ioUyXgpBGLYjLtMIFRFFtV+h8QoRIi3TaYsb2GgWrg4=";
      };

      # TODO: Update from fork
      ply = pkgs.fetchFromGitHub {
        owner = "t4ccer";
        repo = "ply";
        rev = "3449b36306d4193825020a541e4bdb18a113a5d9";
        hash = "sha256-yp6dkcfgt+r2xhiN+JZirHknCaMNChRpfHhO26Vf620=";
      };

      uplc-benchmark-tests =
        config.libHaskell.mkPackage (config.libPlutarch.mkPackage {
          name = "uplc-benchmark-tests";
          src = ./.;
          ghcVersion = "ghc928";
          externalDependencies = [
            self'.packages.uplc-benchmark-types-plutarch
            "${liqwid-libs}/liqwid-plutarch-extra"
            "${liqwid-libs}/liqwid-script-export"
            "${liqwid-libs}/plutarch-benchmark"
            "${liqwid-libs}/plutarch-context-builder"
            "${liqwid-libs}/plutarch-quickcheck"
            "${liqwid-libs}/plutarch-unit"
            "${ply}/ply-core"
            "${ply}/ply-plutarch"
          ];
        });
    in
    {
      checks.uplc-benchmark = uplc-benchmark-tests.checks."uplc-benchmark:test:uplc-benchmark-tests";

      devShells.tests = pkgs.mkShell {
        shellHook = config.pre-commit.installationScript;
        inputsFrom = [
          uplc-benchmark-tests.devShell
        ];
      };
    };
}

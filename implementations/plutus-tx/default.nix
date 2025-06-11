{ inputs
, ...
}:

{
  perSystem = { pkgs, self', config, simpleHaskellNix, ... }:
    let
      plutus-tx-implementation = simpleHaskellNix.mkPackage {
        name = "plutus-tx-implementation";
        src = ./.;

        externalRepositories = {
          "https://chap.intersectmbo.org" = inputs.cardanoPackages;
        };
      };
    in
    {
      devShells.plutus-tx-implementation = pkgs.mkShell {
        shellHook = config.pre-commit.installationScript;
        inputsFrom = [ plutus-tx-implementation.devShell ];
      };

      packages = {
        plutus-tx-implementation-export =
          plutus-tx-implementation.packages."plutus-tx-implementation:exe:plutus-tx-implementation-export";

        plutus-tx-implementation-compiled =
          pkgs.runCommand "plutus-tx-implementation-compiled"
            {
              nativeBuildInputs = [ self'.packages.plutus-tx-implementation-export ];
            } ''
            mkdir -p $out
            cd $out
            plutus-tx-implementation-export
          '';
      };
    };
}

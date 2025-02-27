{
  perSystem = { pkgs, self', config, simpleHaskellNix, ... }:
    let
      cardanoPackages = pkgs.fetchFromGitHub {
        owner = "IntersectMBO";
        repo = "cardano-haskell-packages";
        rev = "3167b742cea332e1c978d8ecc69ef8d6bd0d6e19"; # branch: repo
        hash = "sha256-oCObuK/TY71lL+vDiRT0/Hhrsq4GRC7n8kcKBeonoUk=";
      };

      plutus-tx-implementation = simpleHaskellNix.mkPackage {
        name = "plutus-tx-implementation";
        src = ./.;

        externalRepositories = {
          "https://chap.intersectmbo.org" = cardanoPackages;
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

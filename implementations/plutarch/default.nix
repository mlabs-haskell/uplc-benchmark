{
  perSystem = { self', config, ... }:
    let
      plutarch-implementation =
        config.libHaskell.mkPackage (config.libPlutarch.mkPackage {
          name = "plutarch-implementation";
          src = ./.;
          ghcVersion = "ghc928";
          externalDependencies = [
            self'.packages.uplc-benchmark-types-plutarch-lib
          ];
        });
    in
    {
      devShells.plutarch-implementation = plutarch-implementation.devShell;
      packages.plutarch-implementation =
        plutarch-implementation.packages."plutarch-implementation:exe:plutarch-implementation";
    };
}

{
  perSystem = { ... }: {
    haskell.plutarch-implementation = {
      src = ./.;
      ghcVersion = "ghc928";
    };
  };
}

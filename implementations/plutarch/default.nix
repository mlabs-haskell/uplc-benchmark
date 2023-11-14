{
  perSystem = { ... }: {
    haskell.hello = {
      src = ./.;
      ghcVersion = "ghc928";
    };
  };
}

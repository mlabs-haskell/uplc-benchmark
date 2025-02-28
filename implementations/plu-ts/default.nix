{
  perSystem = { pkgs, config, ... }: {
    devShells.plu-ts-implementation = pkgs.mkShell {
      shellHook = config.pre-commit.installationScript;
      nativeBuildInputs = [
        pkgs.nodejs
      ];
    };
  };
}

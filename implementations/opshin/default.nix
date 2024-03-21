{
  perSystem = { pkgs, config, ... }: {
    devShells.opshin-implementation = pkgs.mkShell {
      shellHook = config.pre-commit.installationScript;
      nativeBuildInputs = [
        pkgs.opshin
        pkgs.xxd
      ];
    };

    packages.opshin-implementation-compiled = pkgs.stdenv.mkDerivation {
      name = "opshin-implementation-compiled";
      src = ./.;

      nativeBuildInputs = [
        pkgs.opshin
        pkgs.xxd
      ];

      enableParallelBuilding = true;
    };
  };
}

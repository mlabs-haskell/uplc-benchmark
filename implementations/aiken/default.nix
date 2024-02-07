{
  perSystem = { pkgs, config, ... }:
    {
      devShells.aiken-implementation = pkgs.mkShell {
        shellHook = config.pre-commit.installationScript;
        nativeBuildInputs = builtins.attrValues config.libAiken.packages;
      };

      packages.aiken-implementation-compiled = config.libAiken.mkPackage {
        name = "aiken-implementation";
        src = ./.;
      };
    };
}

{
  perSystem = { pkgs, config, ... }:
    {
      devShells.aiken-implementation = pkgs.mkShell {
        shellHook = config.pre-commit.installationScript;

        nativeBuildInputs = [
          config.libAiken.aiken
          config.libAiken.aiken2nix
          pkgs.python3
        ];
      };

      packages.aiken-implementation-compiled = config.libAiken.mkPackage {
        name = "aiken-implementation";
        src = ./.;
        aikenLock = ./aiken-nix.lock;

        nativeBuildInputs = [ pkgs.python3 ];
        makeFlags = [ "PREFIX=${placeholder "out"}" ];
      };
    };
}

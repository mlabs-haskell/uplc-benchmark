{
  perSystem = { pkgs, config, ... }:
    let
      aiken-implementation = config.libAiken.mkPackage {
        aikenLock = ./aiken-nix.lock;
        name = "aiken-implementation";
        src = ./.;
      };
    in
    {
      devShells.aiken-implementation = pkgs.mkShell {
        shellHook = config.pre-commit.installationScript;

        nativeBuildInputs = [
          config.libAiken.aiken
          config.libAiken.aiken2nix
        ];
      };
      packages = {
        inherit aiken-implementation;
      };
    };
}

{
  perSystem = { pkgs, config, ... }:
    let
      aiken-implementation = config.libAiken.mkPackage {
        name = "aiken-implementation";
        src = ./.;
        aikenLock = ./aiken-nix.lock;

        nativeBuildInputs = [ pkgs.python3 ];
        makeFlags = [ "PREFIX=${placeholder "out"}" ];
      };
    in
    {
      devShells.aiken-implementation = pkgs.mkShell {
        shellHook = config.pre-commit.installationScript;

        nativeBuildInputs = [
          config.libAiken.aiken
          config.libAiken.aiken2nix
          pkgs.python3
        ];
      };
      packages = {
        inherit aiken-implementation;
      };
    };
}

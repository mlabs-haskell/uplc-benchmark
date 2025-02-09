{
  perSystem = { pkgs, self', config, ... }:
    let
      plutarch-implementation = config.libPlutarch.mkPackage {
        name = "plutarch-implementation";
        src = ./.;
      };
    in
    {
      devShells.plutarch-implementation = plutarch-implementation.devShell;
      packages = {
        plutarch-implementation-export =
          plutarch-implementation.packages."plutarch-implementation:exe:plutarch-implementation-export";

        plutarch-implementation-compiled =
          pkgs.runCommand "plutarch-implementation-compiled"
            {
              nativeBuildInputs = [ self'.packages.plutarch-implementation-export ];
            } ''
            mkdir -p $out
            cd $out
            plutarch-implementation-export
          '';
      };
    };
}

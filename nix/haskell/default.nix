{ lib
, flake-parts-lib
, inputs
, ...
}:
let
  inherit (flake-parts-lib) mkPerSystemOption;
  inherit (lib) types mkOption;
in
{
  options = {
    perSystem = mkPerSystemOption ({ config, system, pkgs, ... }: {
      options = {
        libHaskell = mkOption {
          type = types.anything;
          default = { };
        };
      };

      config =
        let
          mkHaskellPackage = pkgs.callPackage ./lib.nix {
            inherit lib system;
            inherit (inputs) iohk-nix haskell-nix;
          };

        in
        {
          libHaskell = {
            mkPackage = mkHaskellPackage;
          };
        };
    });
  };
}

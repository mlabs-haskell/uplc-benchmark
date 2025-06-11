{ lib
, flake-parts-lib
, inputs
, ...
}:
let
  inherit (flake-parts-lib) mkPerSystemOption;
in
{
  options = {
    perSystem = mkPerSystemOption ({ simpleHaskellNix, config, pkgs, ... }: {
      options = {
        libPlutarch = lib.mkOption {
          type = lib.types.anything;
          default = { };
        };
      };

      config = {
        libPlutarch = {
          mkPackage = pkgs.callPackage ./lib.nix {
            mkHaskellPackage = simpleHaskellNix.mkPackage;
            inherit (inputs) plutarch cardanoPackages;
          };
        };
      };
    });
  };
}

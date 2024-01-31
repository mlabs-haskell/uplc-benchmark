{ lib
, flake-parts-lib
, ...
}:
let
  inherit (flake-parts-lib) mkPerSystemOption;
  inherit (lib) types mkOption;
in
{
  options = {
    perSystem = mkPerSystemOption ({ config, pkgs, ... }:
      let
        libAiken = pkgs.callPackage ./lib.nix { };
      in
      {
        options = {
          libAiken = mkOption {
            type = types.anything;
            default = { };
          };
        };

        config = {
          inherit libAiken;
        };
      });
  };
}

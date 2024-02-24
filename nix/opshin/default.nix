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
        libOpshin = pkgs.callPackage ./lib.nix { };
      in
      {
        options = {
          libOpshin = mkOption {
            type = types.anything;
            default = { };
          };
        };

        config = {
          inherit libOpshin;
        };
      });
  };
}

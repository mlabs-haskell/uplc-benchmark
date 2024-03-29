# Adapted from lambda-buffers repo in a way to reuse the same GHC for everything
{ inputs, lib, ... }:
{
  perSystem = { simpleHaskellNix, pkgs, config, ... }: {
    options = {
      libLb = lib.mkOption {
        type = lib.types.anything;
        default = { };
      };
    };

    config = {
      libLb = pkgs.callPackage ./lib.nix {
        lambda-buffers-src = "${inputs.lambda-buffers}";
        mkPlutarchPackage = config.libPlutarch.mkPackage;
        mkHaskellPackage = simpleHaskellNix.mkPackage;
        inherit (config.libUtils) mkCli;
      };
    };
  };
}

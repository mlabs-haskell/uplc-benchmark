# Adapted from lambda-buffers repo in a way to reuse the same GHC for everything
{ inputs, lib, ... }:
{
  perSystem = { pkgs, config, ... }: {
    options = {
      libLb = lib.mkOption {
        type = lib.types.anything;
        default = { };
      };
    };

    config =
      let
        lb = pkgs.callPackage ./lib.nix {
          lambda-buffers-src = "${inputs.lambda-buffers}";
          mkPlutarchPackage = config.libPlutarch.mkPackage;
          mkHaskellPackage = config.libHaskell.mkPackage;
          inherit (config.libUtils) mkCli;
        };

      in
      {
        libLb = lb.lib;
      };
  };
}

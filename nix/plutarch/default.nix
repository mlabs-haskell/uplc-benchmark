{ lib
, flake-parts-lib
, ...
}:
let
  inherit (flake-parts-lib) mkPerSystemOption;
  inherit (lib) types mkOption;
  configName = "plutarch";
in
{
  options = {
    perSystem = mkPerSystemOption ({ config, pkgs, ... }: {
      options = {
        ${configName} = lib.mkOption {
          type = types.attrsOf (types.submodule ({ ... }: {
            options = {
              src = mkOption {
                type = types.path;
              };

              ghcVersion = mkOption {
                type = types.str;
                example = "ghc945";
              };

              cardanoPackages = mkOption {
                type = types.nullOr types.package;
                default = null;
              };

              plutarchPackage = mkOption {
                type = types.nullOr types.package;
                default = null;
              };

              externalDependencies = mkOption {
                type = types.listOf (types.oneOf [ types.str types.package ]);
                default = [ ];
              };
            };
          }));
          default = { };
        };
        libPlutarch = lib.mkOption {
          type = lib.types.anything;
          default = { };
        };
      };
      config =
        let
          mkPlutarchPackage = pkgs.callPackage ./lib.nix {
            inherit (config.libUtils) applyPatches;
          };
          projects =
            lib.attrsets.mapAttrs
              (config.libUtils.withNameAttr mkPlutarchPackage)
              config.${configName};
        in
        {
          haskell = projects;
          libPlutarch = {
            mkPackage = mkPlutarchPackage;
          };
        };
    });
  };
}

{ lib
, flake-parts-lib
, ...
}:
let
  inherit (flake-parts-lib) mkPerSystemOption;
  inherit (lib) types mkOption;
  conifgName = "plutarch";
in
{
  options = {
    perSystem = mkPerSystemOption ({ config, pkgs, ... }: {
      options = {
        ${conifgName} = lib.mkOption {
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
          mkPlutarchPackage = _name: pkgs.callPackage ./lib.nix { };
          projects = lib.attrsets.mapAttrs mkPlutarchPackage config.${conifgName};
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

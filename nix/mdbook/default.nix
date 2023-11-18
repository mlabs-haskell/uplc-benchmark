{ lib
, flake-parts-lib
, ...
}:
let
  inherit
    (lib)
    types
    mkOption
    ;
  configName = "mdBook";
  libName = "libMdBook";
in
{
  options = {
    perSystem =
      flake-parts-lib.mkPerSystemOption
        ({ config
         , pkgs
         , ...
         }: {
          options = {
            ${configName} = lib.mkOption {
              type = types.attrsOf (types.submodule ({ ... }: {
                options = {
                  src = mkOption {
                    type = types.path;
                    description = ''
                      The source directory of the mdbook source.
                    '';
                  };

                  preBuild = mkOption {
                    type = types.str;
                    default = "";
                    description = ''
                      A command to run before building the mdbook.
                    '';
                  };
                };
              }));

              default = { };
            };

            ${libName} = mkOption {
              type = types.anything;
              default = { };
            };
          };
          config =
            let
              mkMdBookPackage = pkgs.callPackage ./lib.nix { };
            in
            {
              packages =
                lib.mapAttrs
                  (config.libUtils.withNameAttr mkMdBookPackage)
                  config.${configName};

              ${libName} = {
                mkPackage = mkMdBookPackage;
              };
            };
        });
  };
}

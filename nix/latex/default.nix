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
  configName = "latex";
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
              type = types.attrsOf (types.submodule ({ name, ... }: {
                options = {
                  src = mkOption {
                    type = types.path;
                    description = ''
                      The source directory of the LaTeX document.
                    '';
                  };

                  mainFile = mkOption {
                    type = types.str;
                    default = "${builtins.baseNameOf config.latex.${name}.src}.tex";
                    description = ''
                      The main LaTeX file to build.
                    '';
                  };
                };
              }));
            };
            libLatex = lib.mkOption {
              type = lib.types.anything;
              default = { };
            };
          };
          config =
            let
              mkLatexPackage = pkgs.callPackage ./lib.nix { };
            in
            {
              packages =
                lib.mapAttrs (config.libUtils.withNameAttr mkLatexPackage) config.${configName};
              libLatex = {
                mkPackage = mkLatexPackage;
              };
            };
        });
  };
}

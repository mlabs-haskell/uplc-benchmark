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
in
{
  options = {
    perSystem =
      flake-parts-lib.mkPerSystemOption
        ({ config
         , pkgs
         , ...
         }: {
          options.mdbook = lib.mkOption {
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
          };
          config = {
            packages =
              let
                mkMdbookPackage = name: args: pkgs.stdenv.mkDerivation {
                  inherit name;
                  inherit (args) src;

                  nativeBuildInputs = [
                    pkgs.mdbook
                  ];

                  buildPhase = ''
                    runHook preBuild
                    ${args.preBuild}
                    mdbook build . --dest-dir $out
                    runHook postBuild
                  '';

                  dontCheck = true;
                  dontInstall = true;
                  dontFixup = true;
                };

              in
              lib.mapAttrs mkMdbookPackage config.mdbook;
          };
        });
  };
}

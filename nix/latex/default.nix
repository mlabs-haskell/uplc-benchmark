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
          options.latex = lib.mkOption {
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
          config = {
            packages =
              let
                mkLatexPackage = name: args: pkgs.stdenv.mkDerivation {
                  inherit name;
                  inherit (args) src;

                  nativeBuildInputs = [
                    pkgs.texlive.combined.scheme-full
                  ];

                  buildPhase = ''
                    pdflatex -halt-on-error -interaction=nonstopmode "${args.mainFile}"
                  '';

                  dontCheck = true;

                  installPhase = ''
                    mkdir -p $out
                    cp *.pdf $out
                  '';

                  dontFixup = true;
                };

              in
              lib.mapAttrs mkLatexPackage config.latex;
          };
        });
  };
}

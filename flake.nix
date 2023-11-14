{
  description = "uplc-benchmark";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
    hci-effects = {
      url = "github:hercules-ci/hercules-ci-effects";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
    };
  };
  outputs = inputs:
    let
      flakeModules = {
        latex = ./nix/latex;
        mdbook = ./nix/mdbook;
        haskell = ./nix/haskell;
      };
    in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.hci-effects.flakeModule

        ./specifications
        ./website
      ] ++ (builtins.attrValues flakeModules);

      # `nix flake show --impure` hack
      systems =
        if builtins.hasAttr "currentSystem" builtins
        then [ builtins.currentSystem ]
        else inputs.nixpkgs.lib.systems.flakeExposed;

      herculesCI.ciSystems = [ "x86_64-linux" ];

      flake.flakeModules = flakeModules;

      perSystem =
        { config
        , pkgs
        , lib
        , ...
        }: {
          pre-commit.settings = {
            hooks = {
              chktex.enable = true;
              deadnix.enable = true;
              latexindent.enable = true;
              nixpkgs-fmt.enable = true;
              typos.enable = true;
            };

            settings = {
              latexindent.flags = lib.concatStringsSep " "
                [
                  "--yaml=\"defaultIndent:'  ', onlyOneBackUp: 1\""
                  "--local"
                  "--silent"
                  "--overwriteIfDifferent"
                  "--logfile=/dev/null"
                ];
              deadnix.edit = true;
            };

            excludes = [
              ".materialized"
            ];
          };

          devShells.default = pkgs.mkShell {
            shellHook = config.pre-commit.installationScript;
            nativeBuildInputs = [
              pkgs.fd
              pkgs.texlive.combined.scheme-full
              pkgs.mdbook
            ];
          };
        };
    };
}

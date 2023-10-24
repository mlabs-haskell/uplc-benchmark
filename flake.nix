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
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        (import ./nix/latex)
      ];

      # `nix flake show --impure` hack
      systems =
        if builtins.hasAttr "currentSystem" builtins
        then [ builtins.currentSystem ]
        else inputs.nixpkgs.lib.systems.flakeExposed;

      flake.herculesCI.ciSystems = [ "x86_64-linux" ];

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
            };

            settings = {
              deadnix.edit = true;
            };

            # Override settings to use spaces instead of tabs
            # Note that multi line string doesn't work here just because.
            hooks.latexindent.entry = lib.mkForce "${config.pre-commit.settings.tools.latexindent}/bin/latexindent -y=\"defaultIndent:'  ', onlyOneBackUp: 1\" --local --silent --overwrite -g /dev/null";
          };

          devShells.default = pkgs.mkShell {
            shellHook = config.pre-commit.installationScript;
            nativeBuildInputs = [
              pkgs.fd
              pkgs.texlive.combined.scheme-full
            ];
          };

          latex = {
            nft-marketplace-specification.src = ./specifications/nft-marketplace;
          };
        };
    };
}

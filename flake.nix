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
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-2003.follows = "nixpkgs";
        nixpkgs-2105.follows = "nixpkgs";
        nixpkgs-2111.follows = "nixpkgs";
        nixpkgs-2205.follows = "nixpkgs";
        nixpkgs-2211.follows = "nixpkgs";
        nixpkgs-2305.follows = "nixpkgs";
        nixpkgs-unstable.follows = "nixpkgs";
        hydra.follows = "empty-flake";
      };
    };
    lambda-buffers = {
      url = "github:mlabs-haskell/lambda-buffers";
      flake = false;
    };
    empty-flake = {
      url = "github:mlabs-haskell/empty-flake";
    };
  };
  outputs = inputs:
    let
      flakeModules = {
        haskell = ./nix/haskell;
        latex = ./nix/latex;
        mdbook = ./nix/mdbook;
        plutarch = ./nix/plutarch;
        lambdaBuffers = ./nix/lambda-buffers;
        utils = ./nix/utils;
      };
    in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } ({ self, ... }: {
      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.hci-effects.flakeModule

        ./implementations/plutarch
        ./specifications
        ./types
        ./website
        ./tests
      ] ++ (builtins.attrValues flakeModules);

      # `nix flake show --impure` hack
      systems =
        if builtins.hasAttr "currentSystem" builtins
        then [ builtins.currentSystem ]
        else inputs.nixpkgs.lib.systems.flakeExposed;

      herculesCI.ciSystems = [ "x86_64-linux" ];

      hercules-ci.flake-update = {
        enable = true;
        updateBranch = "hci/update-flake-lock";
        createPullRequest = true;
        autoMergeMethod = null;
        when = {
          minute = 45;
          hour = 12;
          dayOfWeek = "Sun";
        };
      };

      flake.flakeModules = flakeModules;

      perSystem =
        { config
        , pkgs
        , lib
        , self'
        , system
        , ...
        }: {
          _module.args.pkgs = import self.inputs.nixpkgs {
            inherit system;
            config.allowBroken = true;
          };

          pre-commit.settings = {
            hooks = {
              chktex.enable = true;
              deadnix.enable = true;
              latexindent.enable = true;
              nixpkgs-fmt.enable = true;
              typos.enable = true;
              fourmolu.enable = true;
            };

            tools = {
              fourmolu = lib.mkForce (pkgs.callPackage ./nix/fourmolu {
                mkHaskellPackage = config.libHaskell.mkPackage;
              });
            };

            settings = {
              latexindent.flags = config.libUtils.mkCli {
                yaml = "\"defaultIndent:'  ', onlyOneBackUp: 1\"";
                local = true;
                silent = true;
                overwriteIfDifferent = true;
                logfile = "/dev/null";
              };
              deadnix.edit = true;
            };

            excludes = [
              ".materialized"
            ];
          };

          devShells = {
            default = pkgs.mkShell {
              shellHook = config.pre-commit.installationScript;

              nativeBuildInputs = [
                pkgs.fd
                pkgs.texlive.combined.scheme-full
                pkgs.mdbook
                self'.packages.lbf-plutus-to-plutarch
              ];
            };
          };
        };
    });
}

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
    simpleHaskellNix = {
      url = "github:mlabs-haskell/simple-haskell-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        pre-commit-hooks-nix.follows = "pre-commit-hooks-nix";
        hci-effects.follows = "hci-effects";
      };
    };
    aiken = {
      url = "github:aiken-lang/aiken?ref=v1.0.24-alpha";
      flake = false;
    };
    plutus-test = {
      url = "github:mlabs-haskell/plutus-test";
      flake = false;
    };
  };
  outputs = inputs:
    let
      flakeModules = {
        aiken = ./nix/aiken;
        lambdaBuffers = ./nix/lambda-buffers;
        latex = ./nix/latex;
        mdbook = ./nix/mdbook;
        plutarch = ./nix/plutarch;
        utils = ./nix/utils;
      };
    in
    inputs.flake-parts.lib.mkFlake { inherit inputs; } ({ self, ... }: {
      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.hci-effects.flakeModule
        inputs.simpleHaskellNix.flakeModules.simpleHaskellNix

        ./implementations/aiken
        ./implementations/opshin
        ./implementations/plutarch
        # ./implementations/plutus-tx
        ./specifications
        # ./types
        # ./website
        # ./tests
      ] ++ (builtins.attrValues flakeModules);

      systems = inputs.nixpkgs.lib.systems.flakeExposed;

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
        , system
        , simpleHaskellNix
        , ...
        }: {
          _module.args.pkgs = import self.inputs.nixpkgs {
            inherit system;
            config.allowBroken = true;
          };

          pre-commit.settings = {
            hooks = {
              aiken-fmt = config.libAiken.preCommit.aikenFmt { enable = true; };
              black.enable = true;
              deadnix.enable = true;
              fourmolu.enable = true;
              latexindent.enable = true;
              nixpkgs-fmt.enable = true;
              typos.enable = true;
            };

            tools = {
              fourmolu = lib.mkForce (pkgs.callPackage ./nix/fourmolu {
                mkHaskellPackage = simpleHaskellNix.mkPackage;
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
              typos.ignored-words = [ "wheres" ];
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
              ];
            };
          };
        };
    });
}

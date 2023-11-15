{ self
, lib
, flake-parts-lib
, ...
}:
let
  inherit (flake-parts-lib) mkPerSystemOption;
  inherit (lib) types mkOption;
  conifgName = "haskell";
in
{
  options = {
    perSystem = mkPerSystemOption ({ config, system, pkgs, ... }: {
      options.${conifgName} = lib.mkOption {
        type = types.attrsOf (types.submodule ({ ... }: {
          options = {
            src = mkOption {
              type = types.path;
            };

            ghcVersion = mkOption {
              type = types.str;
              example = "ghc945";
            };

            indexState = mkOption {
              type = types.str;
              default = "2023-11-01T00:00:00Z";
            };

            cardanoPackages = mkOption {
              type = types.nullOr types.package;
              default = null;
            };

            plutarchPackage = mkOption {
              type = types.nullOr types.package;
              default = null;
            };
          };
        }));
        default = { };
      };
      config =
        let
          iohk-nix = pkgs.fetchFromGitHub {
            owner = "input-output-hk";
            repo = "iohk-nix";
            rev = "4848df60660e21fbb3fe157d996a8bac0a9cf2d6";
            hash = "sha256-ediFkDOBP7yVquw1XtHiYfuXKoEnvKGjTIAk9mC6qxo=";
          };
          mkProject = name: args:
            let
              pkgs = import self.inputs.haskell-nix.inputs.nixpkgs {
                inherit system;
                overlays = [
                  self.inputs.haskell-nix.overlay
                  (import "${iohk-nix}/overlays/crypto")
                ];
              };

              projectPath = args.src;
              projectPathLocal = builtins.head
                (builtins.match "${builtins.storeDir}/[^/]*/?(.*)" (toString projectPath));

              materializedSubDir = "/.materialized";
              materializedPath = projectPath + materializedSubDir;
              materializedPathLocal = projectPathLocal + materializedSubDir;

              planNixFile = "/plan.nix";
              planNixPath = materializedPath + planNixFile;
              planNixPathLocal = materializedPathLocal + planNixFile;

              defaultPlutarch = pkgs.stdenv.mkDerivation (finalArgs: {
                pname = "plutarch-src";
                version = "380df4c8101dd6e0dadc620c1f523f5ae2edbc27"; # branch: master
                src = pkgs.fetchFromGitHub {
                  owner = "Plutonomicon";
                  repo = "plutarch-plutus";
                  rev = finalArgs.version;
                  sha256 = "sha256-jPVA4H3ut8umpzVYWxWjzQZQ6q1l8ikAbW3cZZe29VA=";
                };

                patches = [ ./fix-plutarch.patch ];

                dontBuild = true;

                installPhase = ''
                  mkdir -p "$out"
                  cp -r * "$out"
                '';

                dontFixup = true;
              });

              plutarchPackage =
                if args.plutarchPackage == null
                then defaultPlutarch
                else args.plutarchPackage;

              # plutarchPackage =
              #   if args.plutarchPackage == null
              #   then pkgs.fetchFromGitHub {
              #     owner = "Plutonomicon";
              #     repo = "plutarch-plutus";
              #     rev = "380df4c8101dd6e0dadc620c1f523f5ae2edbc27";
              #     sha256 = "sha256-jPVA4H3ut8umpzVYWxWjzQZQ6q1l8ikAbW3cZZe29VA=";
              #   }
              #   else args.plutarchPackage;

              mkHackage = pkgs.callPackage ./mk-hackage.nix {
                inherit lib;
              };

              customHackages = mkHackage {
                compiler-nix-name = args.ghcVersion;
                srcs = [ plutarchPackage.outPath ];
              };

              cardanoPackages =
                if args.cardanoPackages == null
                then
                  pkgs.fetchFromGitHub
                    {
                      owner = "input-output-hk";
                      repo = "cardano-haskell-packages";
                      rev = "835af81be5bd76342191bd64875dbcbc2c45a39f"; # branch: repo
                      hash = "sha256-ZTBmOWmgYg8jVDB3VFu3VSpBaKOGOkp/0u+M9tyTalk=";
                    }
                else args.cardanoPackages;

              modules = [
                ({ pkgs, ... }: {
                  nonReinstallablePkgs = [
                    "array"
                    "array"
                    "base"
                    "binary"
                    "bytestring"
                    "Cabal"
                    "containers"
                    "deepseq"
                    "directory"
                    "exceptions"
                    "filepath"
                    "ghc"
                    "ghc-bignum"
                    "ghc-boot"
                    "ghc-boot"
                    "ghc-boot-th"
                    "ghc-compact"
                    "ghc-heap"
                    "ghcjs-prim"
                    "ghcjs-th"
                    "ghc-prim"
                    "ghc-prim"
                    "hpc"
                    "integer-gmp"
                    "integer-simple"
                    "mtl"
                    "parsec"
                    "pretty"
                    "process"
                    "rts"
                    "stm"
                    "template-haskell"
                    "terminfo"
                    "text"
                    "time"
                    "transformers"
                    "unix"
                    "Win32"
                    "xhtml"
                  ];
                  packages = {
                    cardano-crypto-class.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
                    cardano-crypto-praos.components.library.pkgconfig = pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
                    plutus-simple-model.components.library.setupHaddockFlags = [ "--optghc=-fplugin-opt PlutusTx.Plugin:defer-errors" ];
                  };
                })
              ];

              project = pkgs.haskell-nix.cabalProject {
                src = projectPath;
                name = name;
                compiler-nix-name = args.ghcVersion;

                inputMap = {
                  "https://input-output-hk.github.io/cardano-haskell-packages" = "${cardanoPackages}";
                };
                modules = customHackages.modules ++ modules;
                extra-hackages = customHackages.extra-hackages;
                extra-hackage-tarballs = customHackages.extra-hackage-tarballs;

                index-state = args.indexState;
                plan-sha256 = (import planNixPath).sha256;
                materialized = materializedPath;
              };
              projectFlake = project.flake { };
              calculateSha = project.plan-nix.passthru.calculateMaterializedSha;
              generateMaterialized = project.plan-nix.passthru.generateMaterialized;

              updateScript = pkgs.writeShellScriptBin "update.sh" ''
                echo 'Materializing build plan to speed up Nix evaluation'
                ${generateMaterialized} ${materializedPathLocal}

                echo 'Updating plan SHA'
                echo "{ sha256 = \"`${calculateSha}`\"; }" > ${planNixPathLocal}

                echo 'Remember to add newly created files in ${materializedPathLocal} to git!'
                echo 'You can run `git add ${materializedPathLocal}` to do it'
              '';
            in
            {
              raw = project;
              inherit (projectFlake) packages;
              apps = {
                update.program = updateScript;
              } // projectFlake.apps;
            };
          projects = lib.attrsets.mapAttrs mkProject config.${conifgName};

          flat2With = mkName: xs:
            builtins.listToAttrs
              (lib.flatten
                (lib.mapAttrsToList
                  (nameSpace: attrs:
                    lib.mapAttrsToList
                      (attrName: value: {
                        name = mkName nameSpace attrName;
                        inherit value;
                      })
                      attrs)
                  xs));


          getAttrs = attr: flat2With (a: b: "${a}:${b}")
            (lib.mapAttrs
              (_: project: project.${attr} or { })
              projects);

        in
        {
          packages = getAttrs "packages";
          checks = getAttrs "checks";
          apps = getAttrs "apps";
        };
    });
  };
}

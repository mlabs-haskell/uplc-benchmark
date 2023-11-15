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
              example = "2023-11-01T00:00:00Z";
            };

            externalDependencies = mkOption {
              type = types.listOf (types.oneOf [ types.str types.package ]);
              default = [ ];
            };

            haskellModules = mkOption {
              type = types.listOf types.anything;
              default = [ ];
            };

            externalRepositories = mkOption {
              type = types.attrsOf (types.oneOf [ types.str types.package ]);
              default = { };
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
        in
        let
          pkgs = import self.inputs.haskell-nix.inputs.nixpkgs {
            inherit system;
            overlays = [
              self.inputs.haskell-nix.overlay
              (import "${iohk-nix}/overlays/crypto")
            ];
          };

          mkHackage = pkgs.callPackage ./mk-hackage.nix { };

          mkProject = name: args:
            let
              projectPath = args.src;
              projectPathLocal = builtins.head
                (builtins.match "${builtins.storeDir}/[^/]*/?(.*)" (toString projectPath));

              materializedSubDir = "/.materialized";
              materializedPath = projectPath + materializedSubDir;
              materializedPathLocal = projectPathLocal + materializedSubDir;

              planNixFile = "/plan.nix";
              planNixPath = materializedPath + planNixFile;
              planNixPathLocal = materializedPathLocal + planNixFile;

              customHackages = mkHackage {
                compiler-nix-name = args.ghcVersion;
                srcs = map toString args.externalDependencies;
              };

              # This looks like a noop but without it haskell.nix throws a runtime
              # error about `pkgs` attribute not being present which is nonsense
              # https://input-output-hk.github.io/haskell.nix/reference/library.html?highlight=cabalProject#modules
              haskellModules = map (m: args @ { ... }: m args) args.haskellModules;

              project = pkgs.haskell-nix.cabalProject' {
                src = projectPath;
                name = name;
                compiler-nix-name = args.ghcVersion;

                inputMap = lib.mapAttrs (_: toString) args.externalRepositories;
                modules = customHackages.modules ++ haskellModules;
                inherit (customHackages) extra-hackages extra-hackage-tarballs;

                shell = {
                  withHoogle = true;
                  exactDeps = true;

                  tools = {
                    cabal = { };
                    haskell-language-server = { };
                  };
                };

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
              inherit (projectFlake) packages devShell;
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

          getAttr = attr: (lib.mapAttrs
            (_: project: project.${attr})
            projects);

        in
        {
          packages = getAttrs "packages";
          devShells = getAttr "devShell";
          checks = getAttrs "checks";
          apps = getAttrs "apps";
        };
    });
  };
}

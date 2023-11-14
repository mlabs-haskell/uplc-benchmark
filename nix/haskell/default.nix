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
    perSystem = mkPerSystemOption ({ config, system, ... }: {
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
              default = "2023-01-01T00:00:00Z";
            };
          };
        }));
        default = { };
      };
      config =
        let
          mkProject = name: args:
            let
              pkgs = import self.inputs.haskell-nix.inputs.nixpkgs {
                inherit system;
                overlays = [ self.inputs.haskell-nix.overlay ];
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

              project = pkgs.haskell-nix.cabalProject {
                src = projectPath;
                name = name;
                compiler-nix-name = args.ghcVersion;
                inputMap = { };
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

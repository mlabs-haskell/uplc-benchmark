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
          # These two lets are important to fetch `iohk-nix` using pure nixpkgs
          # otherwise you'll get infinite recursion when applying an overlay
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
              customHackages = mkHackage {
                compiler-nix-name = args.ghcVersion;
                srcs = map toString args.externalDependencies;
              };

              # This looks like a noop but without it haskell.nix throws a runtime
              # error about `pkgs` attribute not being present which is nonsense
              # https://input-output-hk.github.io/haskell.nix/reference/library.html?highlight=cabalProject#modules
              haskellModules = map (m: args @ { ... }: m args) args.haskellModules;

              project = pkgs.haskell-nix.cabalProject' {
                inherit (args) src;
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
              };
              projectFlake = project.flake { };
            in
            {
              inherit (projectFlake) packages devShell devShells checks apps;
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

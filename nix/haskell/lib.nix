{ iohk-nix
, lib
, system
, haskell-nix
}:

let
  pkgs = import haskell-nix.inputs.nixpkgs {
    inherit system;
    overlays = [
      haskell-nix.overlay
      (import "${iohk-nix}/overlays/crypto")
    ];
  };

  mkHackage = pkgs.callPackage ./mk-hackage.nix { };
in

{ name
, src
, ghcVersion
, haskellModules ? [ ]
, externalDependencies ? [ ]
, externalRepositories ? { }
}:
let
  # This looks like a noop but without it haskell.nix throws a runtime
  # error about `pkgs` attribute not being present which is nonsense
  # https://input-output-hk.github.io/haskell.nix/reference/library.html?highlight=cabalProject#modules
  fixedHaskellModules = map (m: args @ { ... }: m args) haskellModules;

  flatExternalDependencies =
    lib.lists.concatMap
      (dep: [ dep ] ++
        (flatExternalDependencies (dep.passthru or { }).externalDependencies or [ ]));
  flattenedExternalDependencies = flatExternalDependencies externalDependencies;

  # FIXME: make mkHackage work with empty input as well
  extraDependencies =
    if builtins.length flattenedExternalDependencies == 0
    then { modules = fixedHaskellModules; }
    else
      let
        customHackages = mkHackage {
          compiler-nix-name = ghcVersion;
          srcs = map toString flattenedExternalDependencies;
        };
      in
      {
        modules = customHackages.modules ++ fixedHaskellModules;
        inherit (customHackages) extra-hackages extra-hackage-tarballs;
      };

  project = pkgs.haskell-nix.cabalProject' ({
    inherit src;
    name = name;

    compiler-nix-name = ghcVersion;
    inputMap = lib.mapAttrs (_: toString) externalRepositories;

    shell = {
      withHoogle = true;
      exactDeps = true;

      tools = {
        cabal = { };
        haskell-language-server = { };
      };
    };
  } // extraDependencies);
  projectFlake = project.flake { };
in
projectFlake

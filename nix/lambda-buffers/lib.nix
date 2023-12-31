# Adapted from lambda-buffers repo in a way to reuse the same GHC for everything
{ callPackage
, fetchFromGitHub
, writeShellScriptBin
  # Custom
, mkHaskellPackage
, mkPlutarchPackage
, mkCli
, lambda-buffers-src # : path
}:

let
  proto-lens-protoc = (mkHaskellPackage {
    name = "proto-lens-protoc";
    src = (fetchFromGitHub {
      owner = "google";
      repo = "proto-lens";
      rev = "cb73d5029bf2a82098591fe84f6623f31329f1d0";
      hash = "sha256-l0Cu/G/Zh6GDLFUZRKZuhfYEKT0lSeLK26nJooiDFsw=";
    }).outPath + "/proto-lens-protoc";
    ghcVersion = "ghc928";
  }).packages."proto-lens-protoc:exe:proto-lens-protoc";

  haskellProto = callPackage ./haskell-proto.nix {
    inherit proto-lens-protoc;
    inherit mkCli;
  };


  mkProtoLib = { name, cabalBuildInputs ? [ ] }: haskellProto {
    src = "${lambda-buffers-src}/api";
    proto = "${name}.proto";
    cabalPackageName = "lambda-buffers-${name}-pb";
    inherit cabalBuildInputs;
  };

  protoLibs = rec {
    lang = mkProtoLib {
      name = "lang";
    };

    compiler = mkProtoLib {
      name = "compiler";
      cabalBuildInputs = [ lang ];
    };

    codegen = mkProtoLib {
      name = "codegen";
      cabalBuildInputs = [ lang ];
    };
  };

  haskellPackages = {
    compiler = mkHaskellPackage {
      name = "lambda-buffers-compiler";
      src = "${lambda-buffers-src}/lambda-buffers-compiler";
      ghcVersion = "ghc928";
      externalDependencies = builtins.attrValues protoLibs;
    };

    frontend = mkHaskellPackage {
      name = "lambda-buffers-frontend";
      src = "${lambda-buffers-src}/lambda-buffers-frontend";
      ghcVersion = "ghc928";
      externalDependencies = builtins.attrValues protoLibs ++ [
        "${lambda-buffers-src}/lambda-buffers-compiler"
      ];
    };

    codegen = mkHaskellPackage {
      name = "lambda-buffers-codegen";
      src = "${lambda-buffers-src}/lambda-buffers-codegen";
      ghcVersion = "ghc928";
      externalDependencies = builtins.attrValues protoLibs ++ [
        "${lambda-buffers-src}/lambda-buffers-compiler"
      ];
    };
  };

  executables = {
    compiler = haskellPackages.compiler.packages."lambda-buffers-compiler:exe:lbc";
    frontend = haskellPackages.frontend.packages."lambda-buffers-frontend:exe:lbf";
    codegen = haskellPackages.codegen.packages."lambda-buffers-codegen:exe:lbg";
  };

  mkSimpleLambdaBuffersLibrary = name: "${lambda-buffers-src}/libs/lbf-${name}";

  lbf-prelude = mkSimpleLambdaBuffersLibrary "prelude";
  lbf-plutus = mkSimpleLambdaBuffersLibrary "plutus";

  mkSimpleGen = backend: writeShellScriptBin "lbg-${backend}" ''
    ${executables.codegen}/bin/lbg gen-${backend} $@
  '';

  lbg-haskell = mkSimpleGen "haskell";
  lbg-plutarch = mkSimpleGen "plutarch";

  codegenConfigs = "${lambda-buffers-src}/lambda-buffers-codegen/data";

  mkLbfCall =
    { gen
    , scriptName ? "lbf-generic-call"
    , imports ? [ ]
    , classes ? [ ]
    , configs ? [ ]
    }:
    let
      flags = mkCli {
        "gen" = gen;
        "import-path" = imports;
        "gen-class" = classes;
        "gen-opt=--config" = configs;
        "work-dir" = ".work";
        "gen-dir" = "autogen";
      };
    in
    writeShellScriptBin scriptName ''
      export LB_COMPILER=${executables.compiler}/bin/lbc
      mkdir autogen
      mkdir .work
      ${executables.frontend}/bin/lbf build ${flags} "$@"
    '';

  lbf-plutus-to-plutarch = mkLbfCall {
    scriptName = "lbf-plutus-to-plutarch";
    gen = "${lbg-plutarch}/bin/lbg-plutarch";

    imports = [
      lbf-prelude
      lbf-plutus
    ];

    classes = [
      "Plutus.V1.PlutusData"
      "Prelude.Json"
      "Prelude.Eq"
    ];

    configs = [
      "${codegenConfigs}/plutarch-prelude.json"
      "${codegenConfigs}/plutarch-plutus.json"
    ];
  };

  lbf-plutus-to-haskell = mkLbfCall {
    scriptName = "lbf-plutus-to-haskell";
    gen = "${lbg-haskell}/bin/lbg-haskell";

    imports = [
      lbf-prelude
      lbf-plutus
    ];

    classes = [
      "Plutus.V1.PlutusData"
      "Prelude.Json"
      "Prelude.Eq"
    ];

    configs = [
      "${codegenConfigs}/haskell-prelude-base.json"
      "${codegenConfigs}/haskell-plutus-plutustx.json"
    ];
  };

  mkLbHaskellPackage = callPackage ./mk-haskell-lib.nix { };

  lbf-prelude-plutarch = mkLbHaskellPackage {
    name = "lbf-prelude-plutarch";
    src = "${lbf-prelude}";
    files = [ "Prelude.lbf" ];
    cabalBuildDepends = [ "base" "lbr-plutarch" "plutarch" ];

    lbfGen = mkLbfCall {
      gen = "${lbg-plutarch}/bin/lbg-plutarch";
      imports = [ lbf-prelude lbf-plutus ];
      classes = [ "Prelude.Eq" ];
      configs = [ "${codegenConfigs}/plutarch-prelude.json" ];
    };
  };

  lbf-prelude-haskell = mkLbHaskellPackage {
    name = "lbf-prelude-haskell";
    src = "${lbf-prelude}";
    files = [ "Prelude.lbf" ];
    cabalBuildDepends = [ "base" "text" "lbr-prelude" ];

    lbfGen = mkLbfCall {
      gen = "${lbg-haskell}/bin/lbg-haskell";
      classes = [ "Prelude.Eq" "Prelude.Json" ];
      configs = [ "${codegenConfigs}/haskell-prelude-base.json" ];
    };
  };

  lbf-plutus-plutarch = mkLbHaskellPackage {
    name = "lbf-plutus-plutarch";
    src = lbf-plutus;
    files = [ "Plutus/V1.lbf" "Plutus/V2.lbf" ];
    cabalBuildDepends = [ "base" "lbr-plutarch" "plutarch" "lbf-prelude-plutarch" ];

    lbfGen = mkLbfCall {
      gen = "${lbg-plutarch}/bin/lbg-plutarch";
      imports = [ lbf-prelude ];
      classes = [ "Prelude.Eq" "Plutus.V1.PlutusData" ];
      configs = [
        "${codegenConfigs}/plutarch-prelude.json"
        "${codegenConfigs}/plutarch-plutus.json"
      ];
    };
  };

  lbr-plutarch = "${lambda-buffers-src}/runtimes/haskell/lbr-plutarch";
  lbr-prelude = "${lambda-buffers-src}/runtimes/haskell/lbr-prelude";

  mkLbPlutarchPackage =
    { name
    , src
    , files ? null
    }: mkHaskellPackage (mkPlutarchPackage {
      inherit name;
      src = mkLbHaskellPackage {
        name = "${name}-lb";
        inherit src files;
        cabalBuildDepends = [
          "base"
          "lbr-plutarch"
          "plutarch"
          "plutus-core < 1.4" # plutarch is broken so here we go
          "lbf-prelude-plutarch"
          "lbf-plutus-plutarch"
        ];
        lbfGen = lbf-plutus-to-plutarch;
      };
      ghcVersion = "ghc928";
      externalDependencies = [
        lbr-plutarch
        lbr-prelude
        lbf-prelude-plutarch
        lbf-plutus-plutarch
      ];
    });

in
{
  packages = {
    inherit lbf-prelude-plutarch lbf-prelude-haskell lbf-plutus-plutarch;
    inherit lbf-plutus-to-plutarch lbf-plutus-to-haskell;
  };

  lib = {
    inherit mkLbHaskellPackage mkLbfCall mkLbPlutarchPackage;
  };
}

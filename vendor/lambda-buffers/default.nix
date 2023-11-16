# Adapted from lambda-buffers repo in a way to reuse the same GHC for everything
{ lib, ... }:
{
  perSystem = { pkgs, config, ... }: {
    options = {
      mkLbHaskellPackage = lib.mkOption {
        type = lib.types.anything;
        default = { };
      };

      mkLbfCall = lib.mkOption {
        type = lib.types.anything;
        default = { };
      };
    };

    config =
      let
        linkToOut = { name, src }: pkgs.stdenv.mkDerivation {
          inherit name src;
          phases = "installPhase";
          installPhase = "cp -r $src $out";
        };

        proto-lens-protoc = (config.libHaskell.mkPackage {
          name = "proto-lens-protoc";
          src = (pkgs.fetchFromGitHub {
            owner = "google";
            repo = "proto-lens";
            rev = "cb73d5029bf2a82098591fe84f6623f31329f1d0";
            hash = "sha256-l0Cu/G/Zh6GDLFUZRKZuhfYEKT0lSeLK26nJooiDFsw=";
          }).outPath + "/proto-lens-protoc";
          ghcVersion = "ghc928";
        }).packages."proto-lens-protoc:exe:proto-lens-protoc";

        haskellProto = pkgs.callPackage ./haskell-proto.nix {
          inherit proto-lens-protoc;
        };

        lambda-buffers-src = pkgs.fetchFromGitHub {
          owner = "mlabs-haskell";
          repo = "lambda-buffers";
          rev = "c7330e1826c74414d3870a2e511b53da03e8f854";
          hash = "sha256-jWdPvPpMj9ghM+TyF9NuySQEIf8s7UoEfeaBYwk2quk=";
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
          compiler = config.libHaskell.mkPackage {
            name = "lambda-buffers-compiler";
            src = "${lambda-buffers-src}/lambda-buffers-compiler";
            ghcVersion = "ghc928";
            externalDependencies = builtins.attrValues protoLibs;
          };

          frontend = config.libHaskell.mkPackage {
            name = "lambda-buffers-frontend";
            src = "${lambda-buffers-src}/lambda-buffers-frontend";
            ghcVersion = "ghc928";
            externalDependencies = builtins.attrValues protoLibs ++ [
              "${lambda-buffers-src}/lambda-buffers-compiler"
            ];
          };

          codegen = config.libHaskell.mkPackage {
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

        mkSimpleLambdaBuffersLibrary = name: linkToOut {
          name = "lbf-${name}";
          src = "${lambda-buffers-src}/libs/lbf-${name}";
        };

        lbf-prelude = mkSimpleLambdaBuffersLibrary "prelude";
        lbf-plutus = mkSimpleLambdaBuffersLibrary "plutus";

        mkSimpleGen = backend: pkgs.writeShellScriptBin "lbg-${backend}" ''
          ${executables.codegen}/bin/lbg gen-${backend} $@
        '';

        lbg-haskell = mkSimpleGen "haskell";
        lbg-plutarch = mkSimpleGen "plutarch";

        codegenConfigs = linkToOut {
          name = "codegen-configs";
          src = "${lambda-buffers-src}/lambda-buffers-codegen/data";
        };

        mkLbfCall = { gen, imports ? [ ], classes ? [ ], configs ? [ ] }:
          let
            genFlag = "--gen=${gen}";
            importPathFlags = builtins.concatStringsSep " " (map (f: "--import-path=${f}") imports);
            classesFlags = builtins.concatStringsSep " " (map (f: "--gen-class='${f}'") classes);
            configFlags = builtins.concatStringsSep " " (map (f: "--gen-opt=--config=${f}") configs);
            flags = builtins.concatStringsSep " " [ genFlag importPathFlags classesFlags configFlags ];
          in
          pkgs.writeShellScriptBin "lbf-call" ''
            export LB_COMPILER=${executables.compiler}/bin/lbc
            mkdir autogen
            mkdir .work
            ${executables.frontend}/bin/lbf build ${flags} --work-dir=.work --gen-dir=autogen "$@"
          '';

        lbf-plutus-to-plutarch = mkLbfCall {
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

        mkLbHaskellPackage = pkgs.callPackage ./mk-haskell-lib.nix { };

        lbf-prelude-plutarch = mkLbHaskellPackage {
          name = "lbf-prelude-plutarch";
          src = lbf-prelude.outPath;
          files = [ "Prelude.lbf" ];
          exposedModules = [ "LambdaBuffers.Prelude.Plutarch" ];
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
          src = lbf-prelude.outPath;
          files = [ "Prelude.lbf" ];
          exposedModules = [ "LambdaBuffers.Prelude" ];
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
          exposedModules = [
            "LambdaBuffers.Plutus.V1.Plutarch"
            "LambdaBuffers.Plutus.V2.Plutarch"
          ];
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

        lbr-plutarch = linkToOut {
          name = "lbr-plutarch";
          src = "${lambda-buffers-src}/runtimes/haskell/lbr-plutarch";
        };

        lbr-prelude = linkToOut {
          name = "lbr-plutarch";
          src = "${lambda-buffers-src}/runtimes/haskell/lbr-prelude";
        };

      in
      {
        packages = {
          inherit lbf-prelude-plutarch lbf-prelude-haskell lbf-plutus-plutarch;
          inherit lbf-plutus-to-plutarch lbf-plutus-to-haskell;
          inherit lbr-plutarch lbr-prelude;
        };

        inherit mkLbHaskellPackage mkLbfCall;
      };
  };
}

# Adapted from lambda-buffers repo in a way to reuse the same GHC for everything
{
  perSystem = { pkgs, config, ... }:
    let
      proto-lens-protoc = (config.mkHaskellPackage {
        name = "proto-lens-protoc";
        src = (pkgs.fetchFromGitHub {
          owner = "google";
          repo = "proto-lens";
          rev = "cb73d5029bf2a82098591fe84f6623f31329f1d0";
          hash = "sha256-l0Cu/G/Zh6GDLFUZRKZuhfYEKT0lSeLK26nJooiDFsw=";
        }).outPath + "/proto-lens-protoc";
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

      lambda-buffers-lang-hs-pb = haskellProto {
        src = "${lambda-buffers-src}/api";
        proto = "lang.proto";
        cabalPackageName = "lambda-buffers-lang-pb";
      };

      lambda-buffers-compiler-hs-pb = haskellProto {
        src = "${lambda-buffers-src}/api";
        proto = "compiler.proto";
        cabalPackageName = "lambda-buffers-compiler-pb";
        cabalBuildInputs = [ lambda-buffers-lang-hs-pb ];
      };

      lambda-buffers-codegen-hs-pb = haskellProto {
        src = "${lambda-buffers-src}/api";
        proto = "codegen.proto";
        cabalPackageName = "lambda-buffers-codegen-pb";
        cabalBuildInputs = [ lambda-buffers-lang-hs-pb ];
      };

      lbc = (config.mkHaskellPackage {
        name = "lambda-buffers-compiler";
        src = "${lambda-buffers-src}/lambda-buffers-compiler";

        externalDependencies = [
          lambda-buffers-lang-hs-pb
          lambda-buffers-compiler-hs-pb
          lambda-buffers-codegen-hs-pb
        ];
      }).packages."lambda-buffers-compiler:exe:lbc";


      lbf = (config.mkHaskellPackage {
        name = "lambda-buffers-frontend";
        src = "${lambda-buffers-src}/lambda-buffers-frontend";

        externalDependencies = [
          lambda-buffers-lang-hs-pb
          lambda-buffers-compiler-hs-pb
          lambda-buffers-codegen-hs-pb
          "${lambda-buffers-src}/lambda-buffers-compiler"
        ];
      }).packages."lambda-buffers-frontend:exe:lbf";

      lbg = (config.mkHaskellPackage {
        name = "lambda-buffers-codegen";
        src = "${lambda-buffers-src}/lambda-buffers-codegen";

        externalDependencies = [
          lambda-buffers-lang-hs-pb
          lambda-buffers-compiler-hs-pb
          lambda-buffers-codegen-hs-pb
          "${lambda-buffers-src}/lambda-buffers-compiler"
        ];
      }).packages."lambda-buffers-codegen:exe:lbg";

      mkSimpleLambdaBuffersLibrary = name: pkgs.stdenv.mkDerivation {
        name = "lbf-${name}";
        src = "${lambda-buffers-src}/libs/lbf-${name}";
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

      lbf-prelude = mkSimpleLambdaBuffersLibrary "prelude";
      lbf-plutus = mkSimpleLambdaBuffersLibrary "plutus";

      lbg-haskell = pkgs.writeShellScriptBin "lbg-haskell" ''
        ${lbg}/bin/lbg gen-haskell $@
      '';

      codegen-configs = pkgs.stdenv.mkDerivation {
        name = "codegen-configs";
        src = "${lambda-buffers-src}/lambda-buffers-codegen/data";
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };

      lbf-plutus-to-plutarch = pkgs.writeShellScriptBin "lbf-plutus-to-plutarch" ''
        export LB_COMPILER=${config.packages.lbc}/bin/lbc
        mkdir autogen
        mkdir .work
        ${lbf}/bin/lbf build                                                 \
          --import-path=${lbf-prelude}                                       \
          --import-path=${lbf-plutus}                                        \
          --work-dir=.work                                                   \
          --gen=${lbg-haskell}/bin/lbg-haskell                               \
          --gen-class="Prelude.Eq"                                           \
          --gen-class="Prelude.Json"                                         \
          --gen-class="Plutus.V1.PlutusData"                                 \
          --gen-dir=autogen                                                  \
          --gen-opt=--config=${codegen-configs}/haskell-prelude-base.json    \
          --gen-opt=--config=${codegen-configs}/haskell-plutus-plutustx.json \
          "$@"
      '';

    in
    {
      packages = {
        inherit lbf lbc lbf-plutus-to-plutarch;
      };
    };
}

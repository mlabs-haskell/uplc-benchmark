{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = { with-inline-r = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "plutus-core"; version = "1.3.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@iohk.io";
      author = "Plutus Core Team";
      homepage = "";
      url = "";
      synopsis = "Language library for Plutus Core";
      description = "Pretty-printer, parser, and typechecker for Plutus Core.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bimap" or (errorHandler.buildDepError "bimap"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cardano-crypto" or (errorHandler.buildDepError "cardano-crypto"))
          (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
          (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."composition-prelude" or (errorHandler.buildDepError "composition-prelude"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."dependent-sum-template" or (errorHandler.buildDepError "dependent-sum-template"))
          (hsPkgs."deriving-aeson" or (errorHandler.buildDepError "deriving-aeson"))
          (hsPkgs."deriving-compat" or (errorHandler.buildDepError "deriving-compat"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
          (hsPkgs."plutus-core".components.sublibs.index-envs or (errorHandler.buildDepError "plutus-core:index-envs"))
          (hsPkgs."int-cast" or (errorHandler.buildDepError "int-cast"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
          (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
          (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
          (hsPkgs."monoidal-containers" or (errorHandler.buildDepError "monoidal-containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."multiset" or (errorHandler.buildDepError "multiset"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."prettyprinter-configurable" or (errorHandler.buildDepError "prettyprinter-configurable"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."recursion-schemes" or (errorHandler.buildDepError "recursion-schemes"))
          (hsPkgs."plutus-core".components.sublibs.satint or (errorHandler.buildDepError "plutus-core:satint"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."some" or (errorHandler.buildDepError "some"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."th-compat" or (errorHandler.buildDepError "th-compat"))
          (hsPkgs."th-lift" or (errorHandler.buildDepError "th-lift"))
          (hsPkgs."th-lift-instances" or (errorHandler.buildDepError "th-lift-instances"))
          (hsPkgs."th-utilities" or (errorHandler.buildDepError "th-utilities"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."witherable" or (errorHandler.buildDepError "witherable"))
          (hsPkgs."word-array" or (errorHandler.buildDepError "word-array"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "9.0") (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"));
        buildable = true;
        };
      sublibs = {
        "plutus-ir" = {
          depends = [
            (hsPkgs."algebraic-graphs" or (errorHandler.buildDepError "algebraic-graphs"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."dom-lt" or (errorHandler.buildDepError "dom-lt"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."monoidal-containers" or (errorHandler.buildDepError "monoidal-containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."multiset" or (errorHandler.buildDepError "multiset"))
            (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."witherable" or (errorHandler.buildDepError "witherable"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "9.0") (hsPkgs."integer-gmp" or (errorHandler.buildDepError "integer-gmp"));
          buildable = true;
          };
        "plutus-core-execlib" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."monoidal-containers" or (errorHandler.buildDepError "monoidal-containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."plutus-core".components.sublibs.plutus-ir or (errorHandler.buildDepError "plutus-core:plutus-ir"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        "plutus-core-testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."dependent-map" or (errorHandler.buildDepError "dependent-map"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."lazy-search" or (errorHandler.buildDepError "lazy-search"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."multiset" or (errorHandler.buildDepError "multiset"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-ir or (errorHandler.buildDepError "plutus-core:plutus-ir"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."prettyprinter-configurable" or (errorHandler.buildDepError "prettyprinter-configurable"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."quickcheck-transformer" or (errorHandler.buildDepError "quickcheck-transformer"))
            (hsPkgs."size-based" or (errorHandler.buildDepError "size-based"))
            (hsPkgs."Stream" or (errorHandler.buildDepError "Stream"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        "satint" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = true;
          };
        "index-envs" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."nonempty-vector" or (errorHandler.buildDepError "nonempty-vector"))
            (hsPkgs."ral" or (errorHandler.buildDepError "ral"))
            ];
          buildable = true;
          };
        };
      exes = {
        "plc" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-execlib or (errorHandler.buildDepError "plutus-core:plutus-core-execlib"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        "uplc" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."haskeline" or (errorHandler.buildDepError "haskeline"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-execlib or (errorHandler.buildDepError "plutus-core:plutus-core-execlib"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        "pir" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-execlib or (errorHandler.buildDepError "plutus-core:plutus-core-execlib"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-ir or (errorHandler.buildDepError "plutus-core:plutus-ir"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        "debugger" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."brick" or (errorHandler.buildDepError "brick"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
            (hsPkgs."microlens-th" or (errorHandler.buildDepError "microlens-th"))
            (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-execlib or (errorHandler.buildDepError "plutus-core:plutus-core-execlib"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."text-zipper" or (errorHandler.buildDepError "text-zipper"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            ];
          buildable = if system.isWindows then false else true;
          };
        "traceToStacks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "cost-model-budgeting-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."criterion-measurement" or (errorHandler.buildDepError "criterion-measurement"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        "generate-cost-model" = {
          depends = [
            (hsPkgs."aeson-pretty" or (errorHandler.buildDepError "aeson-pretty"))
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."inline-r" or (errorHandler.buildDepError "inline-r"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = if !flags.with-inline-r then false else true;
          };
        "print-cost-model" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      tests = {
        "plutus-core-test" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hex-text" or (errorHandler.buildDepError "hex-text"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."th-lift-instances" or (errorHandler.buildDepError "th-lift-instances"))
            (hsPkgs."th-utilities" or (errorHandler.buildDepError "th-utilities"))
            ];
          buildable = true;
          };
        "untyped-plutus-core-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-crypto-class" or (errorHandler.buildDepError "cardano-crypto-class"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."plutus-core".components.sublibs.index-envs or (errorHandler.buildDepError "plutus-core:index-envs"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        "plutus-ir-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-ir or (errorHandler.buildDepError "plutus-core:plutus-ir"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            ];
          buildable = true;
          };
        "traceToStacks-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        "satint-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."plutus-core".components.sublibs.satint or (errorHandler.buildDepError "plutus-core:satint"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            ];
          buildable = true;
          };
        "index-envs-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."plutus-core".components.sublibs.index-envs or (errorHandler.buildDepError "plutus-core:index-envs"))
            (hsPkgs."nonempty-vector" or (errorHandler.buildDepError "nonempty-vector"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "cost-model-test" = {
          depends = [
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cassava" or (errorHandler.buildDepError "cassava"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."inline-r" or (errorHandler.buildDepError "inline-r"))
            (hsPkgs."mmorph" or (errorHandler.buildDepError "mmorph"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = if !flags.with-inline-r then false else true;
          };
        "index-envs-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."plutus-core".components.sublibs.index-envs or (errorHandler.buildDepError "plutus-core:index-envs"))
            (hsPkgs."nonempty-vector" or (errorHandler.buildDepError "nonempty-vector"))
            (hsPkgs."ral" or (errorHandler.buildDepError "ral"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/plutus-core-1.3.0.0.tar.gz";
      sha256 = "9cfc2738d27aab8da723f2c26030de467021a33b6deeb0657689faaa373c7237";
      });
    }) // {
    package-description-override = "cabal-version:      3.0\nname:               plutus-core\nversion:            1.3.0.0\nlicense:            Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nmaintainer:         michael.peyton-jones@iohk.io\nauthor:             Plutus Core Team\nsynopsis:           Language library for Plutus Core\ndescription:        Pretty-printer, parser, and typechecker for Plutus Core.\ncategory:           Language, Plutus\nbuild-type:         Simple\nextra-doc-files:\n  CHANGELOG.md\n  README.md\n\nextra-source-files:\n  cost-model/data/*.R\n  cost-model/data/benching.csv\n  cost-model/data/builtinCostModel.json\n  cost-model/data/cekMachineCosts.json\n  plutus-core/test/CostModelInterface/defaultCostModelParams.json\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/plutus\n\n-- inline-r is a problematic dependency. It doesn't build with newer\n-- versions of R, so if we depend on it then people need to install\n-- an old R. This is not so bad for people working on plutus itself\n-- (use Nix or work it out), although we may want to eventually\n-- purge it. However, due to a cabal bug (https://github.com/haskell/cabal/issues/4087),\n-- in some cases cabal will require R to be installed _at solving time_,\n-- even though it never wants to build it. This means that the problem\n-- leaks into our downstream dependencies. So our solution is to guard\n-- the dependency behind a flag, off by default, and turn it on for\n-- ourselves locally.\nflag with-inline-r\n  description: Enable build of packages that use `inline-r`.\n  manual:      True\n  default:     False\n\ncommon lang\n  default-language:   Haskell2010\n  default-extensions:\n    DeriveFoldable\n    DeriveFunctor\n    DeriveGeneric\n    DeriveLift\n    DeriveTraversable\n    DerivingStrategies\n    DerivingVia\n    ExplicitForAll\n    FlexibleContexts\n    GeneralizedNewtypeDeriving\n    ImportQualifiedPost\n    ScopedTypeVariables\n    StandaloneDeriving\n\n  ghc-options:\n    -Wall -Wnoncanonical-monad-instances -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wredundant-constraints -Widentities\n    -Wunused-packages -Wmissing-deriving-strategies\n\n-- This contains UPLC+TPLC, PIR must be explicitly included by depending\n-- on the public sub-library.\n-- In due course UPLC and TPLC should be split, with the main library\n-- containing UPLC.\nlibrary\n  import:             lang\n  exposed-modules:\n    Crypto\n    Data.ByteString.Hash\n    Data.Either.Extras\n    Data.MultiSet.Lens\n    PlutusCore\n    PlutusCore.Annotation\n    PlutusCore.Builtin\n    PlutusCore.Builtin.Debug\n    PlutusCore.Builtin.Elaborate\n    PlutusCore.Builtin.Emitter\n    PlutusCore.Check.Normal\n    PlutusCore.Check.Scoping\n    PlutusCore.Check.Uniques\n    PlutusCore.Check.Value\n    PlutusCore.Compiler\n    PlutusCore.Compiler.Erase\n    PlutusCore.Core\n    PlutusCore.Data\n    PlutusCore.DataFilePaths\n    PlutusCore.DeBruijn\n    PlutusCore.Default\n    PlutusCore.Error\n    PlutusCore.Evaluation.Machine.BuiltinCostModel\n    PlutusCore.Evaluation.Machine.Ck\n    PlutusCore.Evaluation.Machine.CostingFun.Core\n    PlutusCore.Evaluation.Machine.CostingFun.JSON\n    PlutusCore.Evaluation.Machine.CostModelInterface\n    PlutusCore.Evaluation.Machine.ExBudget\n    PlutusCore.Evaluation.Machine.ExBudgetingDefaults\n    PlutusCore.Evaluation.Machine.Exception\n    PlutusCore.Evaluation.Machine.ExMemory\n    PlutusCore.Evaluation.Machine.MachineParameters\n    PlutusCore.Evaluation.Machine.MachineParameters.Default\n    PlutusCore.Evaluation.Result\n    PlutusCore.Examples.Builtins\n    PlutusCore.Examples.Data.Data\n    PlutusCore.Examples.Data.InterList\n    PlutusCore.Examples.Data.List\n    PlutusCore.Examples.Data.Pair\n    PlutusCore.Examples.Data.Shad\n    PlutusCore.Examples.Data.TreeForest\n    PlutusCore.Examples.Data.Vec\n    PlutusCore.Examples.Everything\n    PlutusCore.Flat\n    PlutusCore.FsTree\n    PlutusCore.Mark\n    PlutusCore.MkPlc\n    PlutusCore.Name\n    PlutusCore.Normalize\n    PlutusCore.Normalize.Internal\n    PlutusCore.Parser\n    PlutusCore.Pretty\n    PlutusCore.Quote\n    PlutusCore.Rename\n    PlutusCore.Rename.Internal\n    PlutusCore.Rename.Monad\n    PlutusCore.Size\n    PlutusCore.StdLib.Data.Bool\n    PlutusCore.StdLib.Data.ChurchNat\n    PlutusCore.StdLib.Data.Data\n    PlutusCore.StdLib.Data.Function\n    PlutusCore.StdLib.Data.Integer\n    PlutusCore.StdLib.Data.List\n    PlutusCore.StdLib.Data.Nat\n    PlutusCore.StdLib.Data.Pair\n    PlutusCore.StdLib.Data.ScottList\n    PlutusCore.StdLib.Data.ScottUnit\n    PlutusCore.StdLib.Data.Sum\n    PlutusCore.StdLib.Data.Unit\n    PlutusCore.StdLib.Everything\n    PlutusCore.StdLib.Meta\n    PlutusCore.StdLib.Meta.Data.Function\n    PlutusCore.StdLib.Meta.Data.Tuple\n    PlutusCore.StdLib.Type\n    PlutusCore.Subst\n    PlutusCore.TypeCheck\n    PlutusCore.TypeCheck.Internal\n    PlutusCore.Version\n    PlutusPrelude\n    Prettyprinter.Custom\n    Universe\n    UntypedPlutusCore\n    UntypedPlutusCore.Check.Scope\n    UntypedPlutusCore.Check.Uniques\n    UntypedPlutusCore.Core\n    UntypedPlutusCore.Core.Type\n    UntypedPlutusCore.Core.Zip\n    UntypedPlutusCore.DeBruijn\n    UntypedPlutusCore.Evaluation.Machine.Cek\n    UntypedPlutusCore.Evaluation.Machine.Cek.CekMachineCosts\n    UntypedPlutusCore.Evaluation.Machine.Cek.Debug.Driver\n    UntypedPlutusCore.Evaluation.Machine.Cek.Debug.Internal\n    UntypedPlutusCore.Evaluation.Machine.Cek.Internal\n    UntypedPlutusCore.MkUPlc\n    UntypedPlutusCore.Parser\n    UntypedPlutusCore.Rename\n\n  other-modules:\n    Data.Aeson.Flatten\n    Data.Aeson.THReader\n    Data.Functor.Foldable.Monadic\n    GHC.Natural.Extras\n    PlutusCore.Analysis.Definitions\n    PlutusCore.Builtin.HasConstant\n    PlutusCore.Builtin.KnownKind\n    PlutusCore.Builtin.KnownType\n    PlutusCore.Builtin.KnownTypeAst\n    PlutusCore.Builtin.Meaning\n    PlutusCore.Builtin.Polymorphism\n    PlutusCore.Builtin.Runtime\n    PlutusCore.Builtin.TestKnown\n    PlutusCore.Builtin.TypeScheme\n    PlutusCore.Core.Instance\n    PlutusCore.Core.Instance.Eq\n    PlutusCore.Core.Instance.Pretty\n    PlutusCore.Core.Instance.Pretty.Classic\n    PlutusCore.Core.Instance.Pretty.Common\n    PlutusCore.Core.Instance.Pretty.Default\n    PlutusCore.Core.Instance.Pretty.Plc\n    PlutusCore.Core.Instance.Pretty.Readable\n    PlutusCore.Core.Instance.Recursive\n    PlutusCore.Core.Instance.Scoping\n    PlutusCore.Core.Plated\n    PlutusCore.Core.Type\n    PlutusCore.DeBruijn.Internal\n    PlutusCore.Default.Builtins\n    PlutusCore.Default.Universe\n    PlutusCore.Eq\n    PlutusCore.Parser.Builtin\n    PlutusCore.Parser.ParserCommon\n    PlutusCore.Parser.Type\n    PlutusCore.Pretty.Classic\n    PlutusCore.Pretty.ConfigName\n    PlutusCore.Pretty.Default\n    PlutusCore.Pretty.Extra\n    PlutusCore.Pretty.Plc\n    PlutusCore.Pretty.PrettyConst\n    PlutusCore.Pretty.Readable\n    PlutusCore.Pretty.Utils\n    Universe.Core\n    UntypedPlutusCore.Analysis.Definitions\n    UntypedPlutusCore.Analysis.Usages\n    UntypedPlutusCore.Core.Instance\n    UntypedPlutusCore.Core.Instance.Eq\n    UntypedPlutusCore.Core.Instance.Flat\n    UntypedPlutusCore.Core.Instance.Pretty\n    UntypedPlutusCore.Core.Instance.Pretty.Classic\n    UntypedPlutusCore.Core.Instance.Pretty.Default\n    UntypedPlutusCore.Core.Instance.Pretty.Plc\n    UntypedPlutusCore.Core.Instance.Pretty.Readable\n    UntypedPlutusCore.Core.Instance.Recursive\n    UntypedPlutusCore.Core.Plated\n    UntypedPlutusCore.Evaluation.Machine.Cek.EmitterMode\n    UntypedPlutusCore.Evaluation.Machine.Cek.ExBudgetMode\n    UntypedPlutusCore.Mark\n    UntypedPlutusCore.Rename.Internal\n    UntypedPlutusCore.Simplify\n    UntypedPlutusCore.Size\n    UntypedPlutusCore.Subst\n    UntypedPlutusCore.Transform.ForceDelay\n    UntypedPlutusCore.Transform.Inline\n\n  reexported-modules: Data.SatInt\n  hs-source-dirs:\n    plutus-core/src plutus-core/stdlib plutus-core/examples\n    untyped-plutus-core/src prelude\n\n  -- Bound on cardano-crypto-class for the fixed SECP primitives\n  build-depends:\n    , aeson\n    , array\n    , barbies\n    , base                        >=4.9     && <5\n    , base64-bytestring\n    , bimap\n    , bytestring\n    , cardano-crypto\n    , cardano-crypto-class        >=2.0.0.1 && <2.2\n    , cassava\n    , cborg\n    , composition-prelude         >=1.1.0.1\n    , containers\n    , cryptonite\n    , data-default-class\n    , deepseq\n    , dependent-sum-template\n    , deriving-aeson              >=0.2.3\n    , deriving-compat\n    , dlist\n    , exceptions\n    , extra\n    , filepath\n    , flat                        <0.5\n    , free\n    , ghc-prim\n    , hashable                    >=1.4\n    , hedgehog                    >=1.0\n    , index-envs\n    , int-cast\n    , lens\n    , megaparsec\n    , mmorph\n    , mono-traversable\n    , monoidal-containers\n    , mtl\n    , multiset\n    , nothunks                    >=0.1.1\n    , parser-combinators          >=0.4.0\n    , prettyprinter               >=1.1.0.1\n    , prettyprinter-configurable  ^>=1.1\n    , primitive\n    , recursion-schemes\n    , satint\n    , semigroups                  >=0.19.1\n    , serialise\n    , some\n    , template-haskell\n    , text\n    , th-compat\n    , th-lift\n    , th-lift-instances\n    , th-utilities\n    , time\n    , transformers\n    , unordered-containers\n    , witherable\n    , word-array                  ^>=1.1\n\n  if impl(ghc <9.0)\n    build-depends: integer-gmp\n\ntest-suite plutus-core-test\n  import:           lang\n  type:             exitcode-stdio-1.0\n  main-is:          Spec.hs\n  hs-source-dirs:   plutus-core/test\n  ghc-options:      -threaded -rtsopts -with-rtsopts=-N\n  other-modules:\n    CBOR.DataStability\n    Check.Spec\n    CostModelInterface.Spec\n    Evaluation.Machines\n    Evaluation.Spec\n    Names.Spec\n    Normalization.Check\n    Normalization.Type\n    Parser.Spec\n    Pretty.Readable\n    TypeSynthesis.Spec\n\n  default-language: Haskell2010\n  build-depends:\n    , aeson\n    , base                                            >=4.9 && <5\n    , bytestring\n    , containers\n    , data-default-class\n    , filepath\n    , flat                                            <0.5\n    , hedgehog\n    , hex-text\n    , mmorph\n    , mtl\n    , plutus-core:{plutus-core, plutus-core-testlib}  ^>=1.3\n    , prettyprinter\n    , serialise\n    , tasty\n    , tasty-golden\n    , tasty-hedgehog\n    , tasty-hunit\n    , template-haskell\n    , text\n    , th-lift-instances\n    , th-utilities\n\ntest-suite untyped-plutus-core-test\n  import:         lang\n  type:           exitcode-stdio-1.0\n  main-is:        Spec.hs\n  hs-source-dirs: untyped-plutus-core/test\n  ghc-options:    -O2 -threaded -rtsopts -with-rtsopts=-N\n  other-modules:\n    DeBruijn.Common\n    DeBruijn.Scope\n    DeBruijn.Spec\n    DeBruijn.UnDeBruijnify\n    Evaluation.Builtins\n    Evaluation.Builtins.Common\n    Evaluation.Builtins.Definition\n    Evaluation.Builtins.MakeRead\n    Evaluation.Builtins.SignatureVerification\n    Evaluation.Debug\n    Evaluation.FreeVars\n    Evaluation.Golden\n    Evaluation.Machines\n    Evaluation.Regressions\n    Generators\n    Transform.Simplify\n\n  build-depends:\n    , base                                            >=4.9 && <5\n    , bytestring\n    , cardano-crypto-class\n    , flat                                            <0.5\n    , hedgehog\n    , index-envs\n    , lens\n    , mtl\n    , plutus-core:{plutus-core, plutus-core-testlib}  ^>=1.3\n    , pretty-show\n    , prettyprinter\n    , split\n    , tasty\n    , tasty-golden\n    , tasty-hedgehog\n    , tasty-hunit\n    , text\n\nexecutable plc\n  import:         lang\n  main-is:        plc/Main.hs\n  hs-source-dirs: executables\n  build-depends:\n    , base                  >=4.9 && <5\n    , deepseq\n    , lens\n    , optparse-applicative\n    , plutus-core           ^>=1.3\n    , plutus-core-execlib   ^>=1.3\n    , text\n\nexecutable uplc\n  import:         lang\n  main-is:        uplc/Main.hs\n  hs-source-dirs: executables\n  build-depends:\n    , base                  >=4.9 && <5\n    , deepseq\n    , haskeline\n    , mtl\n    , optparse-applicative\n    , plutus-core           ^>=1.3\n    , plutus-core-execlib   ^>=1.3\n    , prettyprinter\n    , split\n    , text\n\n----------------------------------------------\n-- plutus-ir\n----------------------------------------------\n\nlibrary plutus-ir\n  import:          lang\n  visibility:      public\n  exposed-modules:\n    PlutusIR\n    PlutusIR.Analysis.RetainedSize\n    PlutusIR.Compiler\n    PlutusIR.Compiler.Datatype\n    PlutusIR.Compiler.Definitions\n    PlutusIR.Compiler.Names\n    PlutusIR.Core\n    PlutusIR.Core.Instance\n    PlutusIR.Core.Instance.Flat\n    PlutusIR.Core.Instance.Pretty\n    PlutusIR.Core.Instance.Pretty.Readable\n    PlutusIR.Core.Instance.Scoping\n    PlutusIR.Core.Plated\n    PlutusIR.Core.Type\n    PlutusIR.Error\n    PlutusIR.Mark\n    PlutusIR.MkPir\n    PlutusIR.Parser\n    PlutusIR.Purity\n    PlutusIR.Subst\n    PlutusIR.Transform.Beta\n    PlutusIR.Transform.DeadCode\n    PlutusIR.Transform.Inline.CallSiteInline\n    PlutusIR.Transform.Inline.UnconditionalInline\n    PlutusIR.Transform.Inline.Utils\n    PlutusIR.Transform.LetFloatIn\n    PlutusIR.Transform.LetFloatOut\n    PlutusIR.Transform.LetMerge\n    PlutusIR.Transform.NonStrict\n    PlutusIR.Transform.RecSplit\n    PlutusIR.Transform.Rename\n    PlutusIR.Transform.Substitute\n    PlutusIR.Transform.ThunkRecursions\n    PlutusIR.Transform.Unwrap\n    PlutusIR.TypeCheck\n    PlutusIR.TypeCheck.Internal\n\n  hs-source-dirs:  plutus-ir/src\n  other-modules:\n    PlutusIR.Analysis.Dependencies\n    PlutusIR.Analysis.Size\n    PlutusIR.Analysis.Usages\n    PlutusIR.Compiler.Error\n    PlutusIR.Compiler.Let\n    PlutusIR.Compiler.Lower\n    PlutusIR.Compiler.Provenance\n    PlutusIR.Compiler.Recursion\n    PlutusIR.Compiler.Types\n    PlutusIR.Normalize\n\n  build-depends:\n    , algebraic-graphs     >=0.7\n    , base                 >=4.9     && <5\n    , containers\n    , dom-lt\n    , extra\n    , flat                 <0.5\n    , lens\n    , megaparsec\n    , mmorph\n    , monoidal-containers\n    , mtl\n    , multiset\n    , parser-combinators   >=0.4.0\n    , plutus-core          ^>=1.3\n    , prettyprinter        >=1.1.0.1\n    , semigroupoids\n    , semigroups           >=0.19.1\n    , text\n    , transformers\n    , witherable\n\n  if impl(ghc <9.0)\n    build-depends: integer-gmp\n\ntest-suite plutus-ir-test\n  import:         lang\n  type:           exitcode-stdio-1.0\n  main-is:        Spec.hs\n  hs-source-dirs: plutus-ir/test\n  other-modules:\n    GeneratorSpec\n    GeneratorSpec.Builtin\n    GeneratorSpec.Substitution\n    GeneratorSpec.Terms\n    GeneratorSpec.Types\n    NamesSpec\n    ParserSpec\n    TransformSpec\n    TypeSpec\n\n  build-depends:\n    , base                                  >=4.9 && <5\n    , containers\n    , flat                                  <0.5\n    , hashable\n    , hedgehog\n    , lens\n    , mtl\n    , plutus-core-testlib                   ^>=1.3\n    , plutus-core:{plutus-core, plutus-ir}  ^>=1.3\n    , QuickCheck\n    , serialise\n    , tasty\n    , tasty-hedgehog\n    , tasty-quickcheck\n    , text\n    , unordered-containers\n\nexecutable pir\n  import:         lang\n  main-is:        pir/Main.hs\n  hs-source-dirs: executables\n  build-depends:\n    , base                                  >=4.9 && <5\n    , bytestring\n    , cassava\n    , containers\n    , lens\n    , megaparsec\n    , optparse-applicative\n    , plutus-core-execlib                   ^>=1.3\n    , plutus-core:{plutus-core, plutus-ir}  ^>=1.3\n    , text\n    , transformers\n\n----------------------------------------------\n-- support libs\n----------------------------------------------\n\nlibrary plutus-core-execlib\n  import:          lang\n  visibility:      public\n  hs-source-dirs:  executables/src\n  exposed-modules:\n    PlutusCore.Executable.Common\n    PlutusCore.Executable.Parsers\n\n  build-depends:\n    , aeson\n    , base                                                       >=4.9 && <5\n    , bytestring\n    , deepseq\n    , flat                                                       <0.5\n    , lens\n    , megaparsec\n    , monoidal-containers\n    , mtl\n    , optparse-applicative\n    , plutus-core:{plutus-core, plutus-core-testlib, plutus-ir}  ^>=1.3\n    , prettyprinter\n    , text\n\n-- could split this up if we split up the main library for UPLC/PLC/PIR\nlibrary plutus-core-testlib\n  import:          lang\n  visibility:      public\n  hs-source-dirs:  testlib\n  exposed-modules:\n    PlutusCore.Generators.Hedgehog\n    PlutusCore.Generators.Hedgehog.AST\n    PlutusCore.Generators.Hedgehog.Builtin\n    PlutusCore.Generators.Hedgehog.Denotation\n    PlutusCore.Generators.Hedgehog.Entity\n    PlutusCore.Generators.Hedgehog.Interesting\n    PlutusCore.Generators.Hedgehog.Test\n    PlutusCore.Generators.Hedgehog.TypedBuiltinGen\n    PlutusCore.Generators.Hedgehog.TypeEvalCheck\n    PlutusCore.Generators.Hedgehog.Utils\n    PlutusCore.Generators.NEAT.Common\n    PlutusCore.Generators.NEAT.Spec\n    PlutusCore.Generators.NEAT.Term\n    PlutusCore.Generators.NEAT.Type\n    PlutusCore.Generators.QuickCheck\n    PlutusCore.Generators.QuickCheck.Builtin\n    PlutusCore.Generators.QuickCheck.Common\n    PlutusCore.Generators.QuickCheck.GenerateKinds\n    PlutusCore.Generators.QuickCheck.GenerateTypes\n    PlutusCore.Generators.QuickCheck.GenTm\n    PlutusCore.Generators.QuickCheck.ShrinkTypes\n    PlutusCore.Generators.QuickCheck.Substitutions\n    PlutusCore.Generators.QuickCheck.Unification\n    PlutusCore.Generators.QuickCheck.Utils\n    PlutusCore.Test\n    PlutusIR.Generators.AST\n    PlutusIR.Generators.QuickCheck\n    PlutusIR.Generators.QuickCheck.Common\n    PlutusIR.Generators.QuickCheck.GenerateTerms\n    PlutusIR.Generators.QuickCheck.ShrinkTerms\n    PlutusIR.Test\n    Test.Tasty.Extras\n\n  build-depends:\n    , base                                  >=4.9     && <5\n    , bifunctors\n    , bytestring\n    , containers\n    , data-default-class\n    , dependent-map                         >=0.4.0.0\n    , filepath\n    , hedgehog                              >=1.0\n    , lazy-search\n    , lens\n    , mmorph\n    , mtl\n    , multiset\n    , plutus-core:{plutus-core, plutus-ir}  ^>=1.3\n    , prettyprinter                         >=1.1.0.1\n    , prettyprinter-configurable\n    , QuickCheck\n    , quickcheck-instances\n    , quickcheck-transformer\n    , size-based\n    , Stream\n    , tagged\n    , tasty\n    , tasty-golden\n    , tasty-hedgehog\n    , tasty-hunit\n    , text\n\n----------------------------------------------\n-- debugger\n----------------------------------------------\n\nexecutable debugger\n  import:         lang\n  main-is:        Main.hs\n\n  -- brick does not work on windows\n  if os(windows)\n    buildable: False\n\n  hs-source-dirs: executables/debugger\n  other-modules:\n    Draw\n    Event\n    Types\n\n  ghc-options:    -O2 -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n    , base                  >=4.9 && <5\n    , brick\n    , containers\n    , megaparsec\n    , microlens\n    , microlens-th\n    , mono-traversable\n    , mtl\n    , optparse-applicative\n    , plutus-core           ^>=1.3\n    , plutus-core-execlib   ^>=1.3\n    , prettyprinter\n    , text\n    , text-zipper\n    , vty\n\n----------------------------------------------\n-- profiling\n----------------------------------------------\n\nexecutable traceToStacks\n  import:         lang\n  main-is:        Main.hs\n  hs-source-dirs: executables/traceToStacks\n  other-modules:  Common\n  build-depends:\n    , base                  >=4.9 && <5\n    , bytestring\n    , cassava\n    , optparse-applicative\n    , text\n    , vector\n\n-- Tests for functions called by @traceToStacks@.\ntest-suite traceToStacks-test\n  import:           lang\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   executables/traceToStacks\n  default-language: Haskell2010\n  main-is:          TestGetStacks.hs\n  other-modules:    Common\n  build-depends:\n    , base         >=4.9 && <5\n    , bytestring\n    , cassava\n    , tasty\n    , tasty-hunit\n    , text\n    , vector\n\n----------------------------------------------\n-- cost-model\n----------------------------------------------\n\n-- This runs the microbenchmarks used to generate the cost models for built-in\n-- functions, saving the results in a CSV file which must be specified on the\n-- commmand line.  It will take several hours.\nexecutable cost-model-budgeting-bench\n  import:         lang\n  main-is:        Main.hs\n  other-modules:\n    Benchmarks.Bool\n    Benchmarks.ByteStrings\n    Benchmarks.CryptoAndHashes\n    Benchmarks.Data\n    Benchmarks.Integers\n    Benchmarks.Lists\n    Benchmarks.Misc\n    Benchmarks.Nops\n    Benchmarks.Pairs\n    Benchmarks.Strings\n    Benchmarks.Tracing\n    Benchmarks.Unit\n    Common\n    CriterionExtensions\n    Generators\n\n  hs-source-dirs: cost-model/budgeting-bench\n  build-depends:\n    , base                   >=4.9 && <5\n    , bytestring\n    , cardano-crypto-class\n    , criterion\n    , criterion-measurement\n    , deepseq\n    , directory\n    , filepath\n    , hedgehog\n    , mtl\n    , optparse-applicative\n    , plutus-core            ^>=1.3\n    , QuickCheck\n    , quickcheck-instances\n    , random\n    , text\n    , time\n\n-- This reads CSV data generated by cost-model-budgeting-bench, uses R to build\n-- the cost models for built-in functions, and saves them in a specified\n-- JSON file (see the help).  The 'official' cost model should be checked in\n-- in plutus-core/cost-model/data/builtinCostModel.json.\nexecutable generate-cost-model\n  import:         lang\n  main-is:        Main.hs\n  hs-source-dirs: cost-model/create-cost-model\n  ghc-options:    -threaded -rtsopts -with-rtsopts=-N\n\n  if !flag(with-inline-r)\n    buildable: False\n\n  build-depends:\n    , aeson-pretty\n    , barbies\n    , base                  >=4.9     && <5\n    , bytestring\n    , cassava\n    , directory\n    , exceptions\n    , extra\n    , inline-r              >=1.0.0.0\n    , optparse-applicative\n    , plutus-core           ^>=1.3\n    , text\n    , vector\n\n  other-modules:  CreateBuiltinCostModel\n\n-- The cost models for builtins are generated using R and converted into a JSON\n-- form that can later be used to construct Haskell functions.  This tests that\n-- the predictions of the Haskell version are (approximately) identical to the R\n-- ones.  This test is problematic in CI: pretending that it's a benchmark will\n-- prevent it from being run automatically but will still allow us to run it\n-- manually; `cabal bench` also sets the working directory to the root of the\n-- relevant package, which makes it easier to find the cost model data files\n-- (unlike `cabal run` for exectuables, which sets the working directory to the\n-- current shell directory).\nbenchmark cost-model-test\n  import:         lang\n  type:           exitcode-stdio-1.0\n  main-is:        TestCostModels.hs\n  other-modules:  TH\n  hs-source-dirs: cost-model/test cost-model/create-cost-model\n\n  if !flag(with-inline-r)\n    buildable: False\n\n  build-depends:\n    , barbies\n    , base              >=4.9     && <5\n    , bytestring\n    , cassava\n    , exceptions\n    , extra\n    , hedgehog\n    , inline-r          >=1.0.0.0\n    , mmorph\n    , plutus-core       ^>=1.3\n    , template-haskell\n    , text\n    , vector\n\n  other-modules:  CreateBuiltinCostModel\n\nexecutable print-cost-model\n  import:         lang\n  main-is:        Main.hs\n  hs-source-dirs: cost-model/print-cost-model\n  other-modules:  Paths_plutus_core\n  build-depends:\n    , aeson\n    , base        >=4.9 && <5\n    , bytestring\n    , text\n\n----------------------------------------------\n-- satint\n----------------------------------------------\n\nlibrary satint\n  import:          lang\n  exposed-modules: Data.SatInt\n  hs-source-dirs:  satint/src\n  build-depends:\n    , aeson\n    , base              >=4.9 && <5\n    , cassava\n    , deepseq\n    , nothunks\n    , primitive\n    , serialise\n    , template-haskell\n\ntest-suite satint-test\n  import:           lang\n  type:             exitcode-stdio-1.0\n  main-is:          TestSatInt.hs\n  build-depends:\n    , base                        >=4.9 && <5\n    , HUnit\n    , QuickCheck\n    , satint\n    , test-framework\n    , test-framework-hunit\n    , test-framework-quickcheck2\n\n  default-language: Haskell2010\n  hs-source-dirs:   satint/test\n\n----------------------------------------------\n-- index-envs\n----------------------------------------------\n\nlibrary index-envs\n  import:           lang\n  hs-source-dirs:   index-envs/src\n  default-language: Haskell2010\n  exposed-modules:\n    Data.RandomAccessList.Class\n    Data.RandomAccessList.RelativizedMap\n    Data.RandomAccessList.SkewBinary\n    Data.RandomAccessList.SkewBinarySlab\n\n  build-depends:\n    , base             >=4.9 && <5\n    , containers\n    , extra\n    , nonempty-vector\n    , ral              ^>=0.2\n\n-- broken for ral-0.2 conflicts with cardano-binary:recursion-schemes\nbenchmark index-envs-bench\n  import:           lang\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   index-envs/bench\n  default-language: Haskell2010\n  main-is:          Main.hs\n  build-depends:\n    , base             >=4.9     && <5\n    , criterion        >=1.5.9.0\n    , index-envs\n    , nonempty-vector\n    , ral              ^>=0.2\n    , random           >=1.2.0\n\n-- broken for ral-0.2 conflicts with cardano-binary:recursion-schemes\ntest-suite index-envs-test\n  import:           lang\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   index-envs/test\n  default-language: Haskell2010\n  main-is:          Spec.hs\n  other-modules:    RAList.Spec\n  build-depends:\n    , base                  >=4.9 && <5\n    , index-envs\n    , nonempty-vector\n    , QuickCheck\n    , quickcheck-instances\n    , tasty\n    , tasty-quickcheck\n";
    }
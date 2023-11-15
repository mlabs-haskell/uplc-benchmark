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
    flags = {};
    package = {
      specVersion = "3.0";
      identifier = { name = "plutus-tx"; version = "1.3.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@iohk.io";
      author = "Michael Peyton Jones";
      homepage = "";
      url = "";
      synopsis = "Libraries for Plutus Tx and its prelude";
      description = "Libraries for Plutus Tx and its prelude";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."deriving-compat" or (errorHandler.buildDepError "deriving-compat"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-core".components.sublibs.plutus-ir or (errorHandler.buildDepError "plutus-core:plutus-ir"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."th-compat" or (errorHandler.buildDepError "th-compat"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      sublibs = {
        "plutus-tx-testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."plutus-core".components.sublibs.plutus-ir or (errorHandler.buildDepError "plutus-core:plutus-ir"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      tests = {
        "plutus-tx-test" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hedgehog-fn" or (errorHandler.buildDepError "hedgehog-fn"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-core".components.sublibs.plutus-core-testlib or (errorHandler.buildDepError "plutus-core:plutus-core-testlib"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.doctest.components.exes.doctest or (pkgs.buildPackages.doctest or (errorHandler.buildToolDepError "doctest:doctest")))
            ];
          buildable = if compiler.isGhcjs && true || system.isWindows
            then false
            else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/plutus-tx-1.3.0.0.tar.gz";
      sha256 = "087ad439ea54bc378ef9a40ad3279a228cb4b8432fad1506f3d1199ea8e893ac";
      });
    }) // {
    package-description-override = "cabal-version:   3.0\nname:            plutus-tx\nversion:         1.3.0.0\nlicense:         Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nmaintainer:      michael.peyton-jones@iohk.io\nauthor:          Michael Peyton Jones\nsynopsis:        Libraries for Plutus Tx and its prelude\ndescription:     Libraries for Plutus Tx and its prelude\ncategory:        Language\nbuild-type:      Simple\nextra-doc-files: README.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/plutus\n\ncommon lang\n  default-language:   Haskell2010\n  default-extensions:\n    NoImplicitPrelude\n    DeriveFoldable\n    DeriveFunctor\n    DeriveGeneric\n    DeriveLift\n    DeriveTraversable\n    ExplicitForAll\n    GeneralizedNewtypeDeriving\n    ImportQualifiedPost\n    ScopedTypeVariables\n    StandaloneDeriving\n\n  -- See Plutus Tx readme for why we need the following flags:\n  -- -fobject-code -fno-ignore-interface-pragmas and -fno-omit-interface-pragmas\n  ghc-options:\n    -Wall -Wnoncanonical-monad-instances -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wredundant-constraints -Widentities\n    -Wunused-packages -Wmissing-deriving-strategies -fobject-code\n    -fno-ignore-interface-pragmas -fno-omit-interface-pragmas\n\nlibrary\n  import:          lang\n  hs-source-dirs:  src\n  exposed-modules:\n    PlutusTx\n    PlutusTx.Applicative\n    PlutusTx.AssocMap\n    PlutusTx.Base\n    PlutusTx.Bool\n    PlutusTx.Builtins\n    PlutusTx.Builtins.Class\n    PlutusTx.Builtins.Internal\n    PlutusTx.Code\n    PlutusTx.Coverage\n    PlutusTx.Either\n    PlutusTx.Enum\n    PlutusTx.Eq\n    PlutusTx.ErrorCodes\n    PlutusTx.Foldable\n    PlutusTx.Functor\n    PlutusTx.Integer\n    PlutusTx.IsData\n    PlutusTx.IsData.Class\n    PlutusTx.Lattice\n    PlutusTx.Lift\n    PlutusTx.Lift.Class\n    PlutusTx.List\n    PlutusTx.Maybe\n    PlutusTx.Monoid\n    PlutusTx.Numeric\n    PlutusTx.Ord\n    PlutusTx.Plugin.Utils\n    PlutusTx.Prelude\n    PlutusTx.Ratio\n    PlutusTx.Semigroup\n    PlutusTx.Show\n    PlutusTx.Show.TH\n    PlutusTx.Sqrt\n    PlutusTx.TH\n    PlutusTx.These\n    PlutusTx.Trace\n    PlutusTx.Traversable\n    PlutusTx.Utils\n\n  other-modules:\n    PlutusTx.IsData.Instances\n    PlutusTx.IsData.TH\n    PlutusTx.Lift.Instances\n    PlutusTx.Lift.TH\n    PlutusTx.Lift.THUtils\n\n  build-depends:\n    , aeson\n    , base                                  >=4.9      && <5\n    , bytestring\n    , containers\n    , deepseq\n    , deriving-compat\n    , extra\n    , flat                                  <0.5\n    , ghc-prim\n    , hashable\n    , lens\n    , memory\n    , mtl\n    , plutus-core:{plutus-core, plutus-ir}  ^>=1.3\n    , prettyprinter\n    , serialise\n    , template-haskell                      >=2.13.0.0\n    , text\n    , th-abstraction\n    , th-compat\n    , transformers\n\nlibrary plutus-tx-testlib\n  import:          lang\n  visibility:      public\n  hs-source-dirs:  testlib\n  exposed-modules:\n    Hedgehog.Laws.Common\n    Hedgehog.Laws.Eq\n    Hedgehog.Laws.Lattice\n    Hedgehog.Laws.Ord\n    PlutusTx.Test\n\n  build-depends:\n    , base                                                       >=4.9 && <5\n    , filepath\n    , flat                                                       <0.5\n    , hedgehog\n    , lens\n    , mtl\n    , plutus-core:{plutus-core, plutus-core-testlib, plutus-ir}  ^>=1.3\n    , plutus-tx                                                  ^>=1.3\n    , prettyprinter\n    , tagged\n    , tasty\n    , tasty-hedgehog\n    , text\n\ntest-suite plutus-tx-test\n  import:             lang\n\n  if (impl(ghcjs) || os(windows))\n    buildable: False\n\n  type:               exitcode-stdio-1.0\n  main-is:            Spec.hs\n  other-modules:\n    Rational.Laws\n    Rational.Laws.Additive\n    Rational.Laws.Construction\n    Rational.Laws.Eq\n    Rational.Laws.Helpers\n    Rational.Laws.Module\n    Rational.Laws.Multiplicative\n    Rational.Laws.Ord\n    Rational.Laws.Other\n    Rational.Laws.Ring\n    Rational.Laws.Serialization\n    Show.Spec\n\n  hs-source-dirs:     test\n  build-tool-depends: doctest:doctest\n  build-depends:\n    , aeson\n    , base                                            >=4.9 && <5\n    , base16-bytestring\n    , bytestring\n    , cborg\n    , filepath\n    , hedgehog\n    , hedgehog-fn\n    , plutus-core:{plutus-core, plutus-core-testlib}  ^>=1.3\n    , plutus-tx                                       ^>=1.3\n    , pretty-show\n    , serialise\n    , tasty\n    , tasty-hedgehog\n    , tasty-hunit\n    , text\n    , transformers\n";
    }
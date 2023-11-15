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
      identifier = { name = "plutus-ledger-api"; version = "1.3.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@iohk.io";
      author = "Michael Peyton Jones, Jann Mueller";
      homepage = "";
      url = "";
      synopsis = "Interface to the Plutus ledger for the Cardano ledger.";
      description = "Interface to the Plutus scripting support for the Cardano ledger.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      sublibs = {
        "plutus-ledger-api-testlib" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."PyF" or (errorHandler.buildDepError "PyF"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      exes = {
        "evaluation-test" = {
          depends = [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-ledger-api".components.sublibs.plutus-ledger-api-testlib or (errorHandler.buildDepError "plutus-ledger-api:plutus-ledger-api-testlib"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      tests = {
        "plutus-ledger-api-test" = {
          depends = [
            (hsPkgs."barbies" or (errorHandler.buildDepError "barbies"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-ledger-api".components.sublibs.plutus-ledger-api-testlib or (errorHandler.buildDepError "plutus-ledger-api:plutus-ledger-api-testlib"))
            (hsPkgs."plutus-tx".components.sublibs.plutus-tx-testlib or (errorHandler.buildDepError "plutus-tx:plutus-tx-testlib"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/plutus-ledger-api-1.3.0.0.tar.gz";
      sha256 = "b4b5d0ab82af6037f903aa255d307b143dfe6ab3f12c96990c30a29ab0b2b934";
      });
    }) // {
    package-description-override = "cabal-version:   3.0\nname:            plutus-ledger-api\nversion:         1.3.0.0\nlicense:         Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\n\nmaintainer:      michael.peyton-jones@iohk.io\nauthor:          Michael Peyton Jones, Jann Mueller\nsynopsis:        Interface to the Plutus ledger for the Cardano ledger.\ndescription:\n  Interface to the Plutus scripting support for the Cardano ledger.\n\ncategory:        Language\nbuild-type:      Simple\nextra-doc-files: CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/input-output-hk/plutus\n\ncommon lang\n  default-language:   Haskell2010\n  default-extensions:\n    DeriveFoldable\n    DeriveFunctor\n    DeriveGeneric\n    DeriveLift\n    DeriveTraversable\n    DerivingStrategies\n    ExplicitForAll\n    FlexibleContexts\n    GeneralizedNewtypeDeriving\n    ImportQualifiedPost\n    MultiParamTypeClasses\n    ScopedTypeVariables\n    StandaloneDeriving\n\n  -- See Plutus Tx readme for why we need the following flags:\n  -- -fobject-code -fno-ignore-interface-pragmas and -fno-omit-interface-pragmas\n  ghc-options:\n    -Wall -Wnoncanonical-monad-instances -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wredundant-constraints -Widentities\n    -Wunused-packages -Wmissing-deriving-strategies -fobject-code\n    -fno-ignore-interface-pragmas -fno-omit-interface-pragmas\n\nlibrary\n  import:           lang\n  hs-source-dirs:   src\n  default-language: Haskell2010\n  exposed-modules:\n    PlutusLedgerApi.Common\n    PlutusLedgerApi.Common.Versions\n    PlutusLedgerApi.V1\n    PlutusLedgerApi.V1.Address\n    PlutusLedgerApi.V1.Bytes\n    PlutusLedgerApi.V1.Contexts\n    PlutusLedgerApi.V1.Credential\n    PlutusLedgerApi.V1.Crypto\n    PlutusLedgerApi.V1.DCert\n    PlutusLedgerApi.V1.EvaluationContext\n    PlutusLedgerApi.V1.Interval\n    PlutusLedgerApi.V1.ParamName\n    PlutusLedgerApi.V1.Scripts\n    PlutusLedgerApi.V1.Time\n    PlutusLedgerApi.V1.Tx\n    PlutusLedgerApi.V1.Value\n    PlutusLedgerApi.V2\n    PlutusLedgerApi.V2.Contexts\n    PlutusLedgerApi.V2.EvaluationContext\n    PlutusLedgerApi.V2.ParamName\n    PlutusLedgerApi.V2.Tx\n    PlutusLedgerApi.V3\n    PlutusLedgerApi.V3.EvaluationContext\n    PlutusLedgerApi.V3.ParamName\n\n  other-modules:\n    Codec.CBOR.Extras\n    PlutusLedgerApi.Common.Eval\n    PlutusLedgerApi.Common.ParamName\n    PlutusLedgerApi.Common.ProtocolVersions\n    PlutusLedgerApi.Common.SerialisedScript\n    Prettyprinter.Extras\n\n  build-depends:\n    , base               >=4.9 && <5\n    , base16-bytestring  >=1\n    , bytestring\n    , cborg\n    , containers\n    , deepseq\n    , extra\n    , flat               <0.5\n    , lens\n    , mtl\n    , nothunks\n    , plutus-core        ^>=1.3\n    , plutus-tx          ^>=1.3\n    , prettyprinter\n    , serialise\n    , tagged\n    , text\n\nlibrary plutus-ledger-api-testlib\n  import:          lang\n  visibility:      public\n  hs-source-dirs:  testlib\n  exposed-modules:\n    PlutusLedgerApi.Test.EvaluationContext\n    PlutusLedgerApi.Test.EvaluationEvent\n    PlutusLedgerApi.Test.Examples\n    PlutusLedgerApi.Test.Scripts\n\n  build-depends:\n    , base               >=4.9      && <5\n    , base64-bytestring\n    , bytestring\n    , plutus-core        ^>=1.3\n    , plutus-ledger-api  ^>=1.3\n    , plutus-tx          ^>=1.3\n    , prettyprinter\n    , PyF                >=0.11.1.0\n    , serialise\n    , text\n\ntest-suite plutus-ledger-api-test\n  import:         lang\n  type:           exitcode-stdio-1.0\n  main-is:        Spec.hs\n  hs-source-dirs: test\n  other-modules:\n    Spec.CostModelParams\n    Spec.Eval\n    Spec.Interval\n    Spec.NoThunks\n    Spec.Versions\n\n  build-depends:\n    , barbies\n    , base                                                              >=4.9 && <5\n    , bytestring\n    , containers\n    , extra\n    , hedgehog\n    , lens\n    , mtl\n    , nothunks\n    , plutus-core                                                       ^>=1.3\n    , plutus-ledger-api:{plutus-ledger-api, plutus-ledger-api-testlib}  ^>=1.3\n    , plutus-tx:plutus-tx-testlib                                       ^>=1.3\n    , tasty\n    , tasty-hedgehog\n    , tasty-hunit\n    , tasty-quickcheck\n    , text\n\n-- This is a nightly test, so it is an executable instead of test-suite to avoid\n-- running this in CI.\nexecutable evaluation-test\n  import:           lang\n  main-is:          Main.hs\n  hs-source-dirs:   test-onchain-evaluation\n  ghc-options:      -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n    , async\n    , base                       >=4.9 && <5\n    , extra\n    , filepath\n    , mtl\n    , plutus-core                ^>=1.3\n    , plutus-ledger-api          ^>=1.3\n    , plutus-ledger-api-testlib  ^>=1.3\n    , serialise\n    , tasty\n    , tasty-hunit\n\n  default-language: Haskell2010\n";
    }
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
      specVersion = "2.4";
      identifier = { name = "word-array"; version = "1.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "michael.peyton-jones@iohk.io";
      author = "Zachary Churchill, Michael Peyton Jones";
      homepage = "https://github.com/plutus";
      url = "";
      synopsis = "";
      description = "Treat integral types as arrays of smaller integral types";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mono-traversable" or (errorHandler.buildDepError "mono-traversable"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."word-array" or (errorHandler.buildDepError "word-array"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."word-array" or (errorHandler.buildDepError "word-array"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/word-array-1.1.0.0.tar.gz";
      sha256 = "73354e3ec5d70bcf6f0bede83841cf055c5c8eb389c2ee045e7d710605b13671";
      });
    }) // {
    package-description-override = "cabal-version: 2.4\nname:          word-array\nversion:       1.1.0.0\nsynopsis:\ndescription:   Treat integral types as arrays of smaller integral types\nhomepage:      https://github.com/plutus\nlicense:       Apache-2.0\nlicense-file:  LICENSE\nauthor:        Zachary Churchill, Michael Peyton Jones\nmaintainer:    michael.peyton-jones@iohk.io\ncategory:      Data\n\nsource-repository head\n  type:     git\n  location: https://github.com/iohk/plutus\n\ncommon lang\n  default-language:   Haskell2010\n  default-extensions:\n    DeriveFoldable\n    DeriveFunctor\n    DeriveGeneric\n    DeriveLift\n    DeriveTraversable\n    DerivingStrategies\n    DerivingVia\n    ExplicitForAll\n    FlexibleContexts\n    GeneralizedNewtypeDeriving\n    ImportQualifiedPost\n    ScopedTypeVariables\n    StandaloneDeriving\n\n  ghc-options:\n    -Wall -Wnoncanonical-monad-instances -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wredundant-constraints -Widentities\n    -Wunused-packages -Wmissing-deriving-strategies\n\nlibrary\n  import:          lang\n  exposed-modules: Data.Word64Array.Word8\n  build-depends:\n    , base              >=4.13 && <5.0\n    , deepseq\n    , mono-traversable\n\n  hs-source-dirs:  src\n  ghc-options:     -O2\n\ntest-suite test\n  import:         lang\n  type:           exitcode-stdio-1.0\n  hs-source-dirs: test\n  main-is:        Spec.hs\n  build-depends:\n    , base\n    , mono-traversable\n    , QuickCheck\n    , tasty\n    , tasty-quickcheck\n    , vector\n    , word-array        ^>=1.1\n\nbenchmark bench\n  import:         lang\n  type:           exitcode-stdio-1.0\n  build-depends:\n    , base\n    , primitive\n    , tasty-bench\n    , word-array   ^>=1.1\n\n  ghc-options:    -O2\n  hs-source-dirs: bench\n  main-is:        Main.hs\n";
    }
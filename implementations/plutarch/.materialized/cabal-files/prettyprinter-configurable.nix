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
      identifier = {
        name = "prettyprinter-configurable";
        version = "1.1.0.0";
        };
      license = "NONE";
      copyright = "";
      maintainer = "plutus@iohk.io";
      author = "David Luposchainsky, effectfully";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Custom";
      setup-depends = [
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.cabal-doctest or (pkgs.buildPackages.cabal-doctest or (errorHandler.setupDepError "cabal-doctest")))
        (hsPkgs.buildPackages.doctest or (pkgs.buildPackages.doctest or (errorHandler.setupDepError "doctest")))
        ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "prettyprinter-configurable-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."megaparsec" or (errorHandler.buildDepError "megaparsec"))
            (hsPkgs."parser-combinators" or (errorHandler.buildDepError "parser-combinators"))
            (hsPkgs."prettyprinter-configurable" or (errorHandler.buildDepError "prettyprinter-configurable"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-text" or (errorHandler.buildDepError "quickcheck-text"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        "prettyprinter-configurable-doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = if compiler.isGhc && (compiler.version).ge "9.0"
            then false
            else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/prettyprinter-configurable-1.1.0.0.tar.gz";
      sha256 = "142d8593a67de4e1df7b42b97adeea30990504f95db1911ac3d37036d41e5216";
      });
    }) // {
    package-description-override = "cabal-version:      2.4\nname:               prettyprinter-configurable\nversion:            1.1.0.0\n\n-- synopsis:\n-- description:\n-- homepage:            https://github.com/githubuser/prettyprinter-configurable#readme\nlicense-files:\n  LICENSE\n  NOTICE\n\nauthor:             David Luposchainsky, effectfully\nmaintainer:         plutus@iohk.io\ncategory:           User Interfaces, Text\nbuild-type:         Custom\nextra-source-files: README.md\n\ncommon lang\n  default-language:   Haskell2010\n  default-extensions:\n    DeriveFoldable\n    DeriveFunctor\n    DeriveGeneric\n    DeriveLift\n    DeriveTraversable\n    DerivingStrategies\n    DerivingVia\n    ExplicitForAll\n    FlexibleContexts\n    GeneralizedNewtypeDeriving\n    ImportQualifiedPost\n    ScopedTypeVariables\n    StandaloneDeriving\n\n  ghc-options:\n    -Wall -Wnoncanonical-monad-instances -Wincomplete-uni-patterns\n    -Wincomplete-record-updates -Wredundant-constraints -Widentities\n    -Wunused-packages -Wmissing-deriving-strategies\n\ncustom-setup\n  setup-depends:\n    , base           >=4.9 && <5\n    , Cabal\n    , cabal-doctest\n    , doctest\n\nlibrary\n  import:          lang\n  hs-source-dirs:  src\n  exposed-modules:\n    Text.Fixity\n    Text.Fixity.Internal\n    Text.Pretty\n    Text.PrettyBy\n    Text.PrettyBy.Default\n    Text.PrettyBy.Fixity\n    Text.PrettyBy.Internal\n    Text.PrettyBy.Internal.Utils\n    Text.PrettyBy.Monad\n\n  build-depends:\n    , base           >=4.9 && <5\n    , microlens\n    , mtl\n    , prettyprinter\n    , text\n\n  ghc-options:     -O2\n\ntest-suite prettyprinter-configurable-test\n  import:         lang\n  type:           exitcode-stdio-1.0\n  main-is:        Main.hs\n  hs-source-dirs: test\n  other-modules:\n    Default\n    Expr\n    NonDefault\n    Universal\n\n  build-depends:\n    , base                        >=4.9 && <5\n    , megaparsec\n    , parser-combinators\n    , prettyprinter-configurable  ^>=1.1\n    , QuickCheck\n    , quickcheck-text\n    , tasty\n    , tasty-hunit\n    , tasty-quickcheck\n    , text\n\n  ghc-options:    -threaded -rtsopts -with-rtsopts=-N\n\ntest-suite prettyprinter-configurable-doctest\n  import:         lang\n  type:           exitcode-stdio-1.0\n  main-is:        Main.hs\n  hs-source-dirs: doctest\n  build-depends:\n    , base     >=4.9 && <5\n    , doctest\n\n  -- one test fails on 9.2, don't know why\n  if impl(ghc >=9.0)\n    buildable: False\n\n  ghc-options:    -threaded -rtsopts -with-rtsopts=-N\n";
    }
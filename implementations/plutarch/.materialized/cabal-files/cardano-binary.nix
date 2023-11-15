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
      specVersion = "2.2";
      identifier = { name = "cardano-binary"; version = "1.7.0.1"; };
      license = "Apache-2.0";
      copyright = "2019-2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Binary serialization for Cardano";
      description = "This package includes the binary serialization format for Cardano";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
          (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."recursion-schemes" or (errorHandler.buildDepError "recursion-schemes"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cardano-binary" or (errorHandler.buildDepError "cardano-binary"))
            (hsPkgs."cardano-prelude-test" or (errorHandler.buildDepError "cardano-prelude-test"))
            (hsPkgs."cborg" or (errorHandler.buildDepError "cborg"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."formatting" or (errorHandler.buildDepError "formatting"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/cardano-binary-1.7.0.1.tar.gz";
      sha256 = "55e65ce98f91999b64724b383db8cf36a56913e8f9fc80217af532e862131792";
      });
    }) // {
    package-description-override = "cabal-version: 2.2\n\nname:                cardano-binary\nversion:             1.7.0.1\nsynopsis:            Binary serialization for Cardano\ndescription:         This package includes the binary serialization format for Cardano\nlicense:             Apache-2.0\nlicense-files:\n  LICENSE\n  NOTICE\nauthor:              IOHK\nmaintainer:          operations@iohk.io\ncopyright:           2019-2021 IOHK\ncategory:            Currency\nbuild-type:          Simple\nextra-source-files:  README.md\n                     CHANGELOG.md\n\ncommon base                         { build-depends: base >= 4.14 && < 4.19 }\n\ncommon project-config\n  default-language:     Haskell2010\n\n  ghc-options:          -Wall\n                        -Wcompat\n                        -Wincomplete-record-updates\n                        -Wincomplete-uni-patterns\n                        -Wpartial-fields\n                        -Wredundant-constraints\n                        -Wunused-packages\n\nlibrary\n  import:               base, project-config\n  hs-source-dirs:       src\n  exposed-modules:      Cardano.Binary\n  other-modules:        Cardano.Binary.ToCBOR\n                        Cardano.Binary.FromCBOR\n\n                        Cardano.Binary.Serialize\n                        Cardano.Binary.Deserialize\n\n  build-depends:        base\n                      , bytestring\n                      , cborg              >= 0.2.2 && < 0.3\n                      , containers\n                      , data-fix\n                      , formatting\n                      , primitive\n                      , recursion-schemes  >= 5.1   && < 5.3\n                      , safe-exceptions\n                      , tagged\n                      , text\n                      , time\n                      , vector\n\ntest-suite test\n  import:               base, project-config\n  hs-source-dirs:       test\n  main-is:              test.hs\n  type:                 exitcode-stdio-1.0\n\n  other-modules:        Test.Cardano.Binary.SizeBounds\n                        Test.Cardano.Binary.Helpers\n                        Test.Cardano.Binary.Helpers.GoldenRoundTrip\n                        Test.Cardano.Binary.RoundTrip\n                        Test.Cardano.Binary.Serialization\n                        Test.Cardano.Binary.Failure\n\n  build-depends:        base\n                      , bytestring\n                      , cardano-binary\n                      , cardano-prelude-test\n                      , cborg\n                      , containers\n                      , formatting\n                      , hedgehog\n                      , hspec\n                      , pretty-show\n                      , QuickCheck\n                      , quickcheck-instances\n                      , tagged\n                      , text\n                      , time\n                      , vector\n\n  ghc-options:          -threaded\n                        -rtsopts\n\n";
    }
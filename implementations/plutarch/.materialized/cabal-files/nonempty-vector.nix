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
      identifier = { name = "nonempty-vector"; version = "0.2.3"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2019-2023 Emily Pillmore <emilypi@cohomolo.gy>";
      maintainer = "emilypi@cohomolo.gy";
      author = "Emily Pillmore";
      homepage = "https://github.com/emilypi/nonempty-vector";
      url = "";
      synopsis = "Non-empty vectors";
      description = "Performant, non-empty mutable and immutable vectors";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "tasty" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."nonempty-vector" or (errorHandler.buildDepError "nonempty-vector"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/nonempty-vector-0.2.3.tar.gz";
      sha256 = "0a3702141421076416928f73d3ee6c66a920533ad02a9a771bc0bfc404d7aca3";
      });
    }) // {
    package-description-override = "cabal-version:   3.0\nname:            nonempty-vector\nversion:         0.2.3\nsynopsis:        Non-empty vectors\ndescription:     Performant, non-empty mutable and immutable vectors\nhomepage:        https://github.com/emilypi/nonempty-vector\nbug-reports:     https://github.com/emilypi/nonempty-vector/issues\nlicense:         BSD-3-Clause\nlicense-file:    LICENSE\nauthor:          Emily Pillmore\nmaintainer:      emilypi@cohomolo.gy\ncopyright:       (c) 2019-2023 Emily Pillmore <emilypi@cohomolo.gy>\ncategory:        Data\nbuild-type:      Simple\nextra-doc-files:\n  CHANGELOG.md\n  README.md\n\ntested-with:\n  GHC ==8.10.7\n   || ==9.0.2\n   || ==9.2.6\n   || ==9.4.4\n   || ==9.6.3\n   || ==9.8.1\n\nsource-repository head\n  type:     git\n  location: https://github.com/emilypi/nonempty-vector.git\n\nlibrary\n  exposed-modules:\n    Data.Vector.NonEmpty\n    Data.Vector.NonEmpty.Internal\n    Data.Vector.NonEmpty.Mutable\n\n  build-depends:\n      base       >=4.14  && <4.20\n    , deepseq\n    , primitive  >=0.6  && <0.9\n    , vector     >=0.12 && <0.14\n\n  hs-source-dirs:   src\n  default-language: Haskell2010\n  ghc-options:      -Wall\n\ntest-suite tasty\n  default-language:  Haskell2010\n  type:              exitcode-stdio-1.0\n  main-is:           Main.hs\n  build-depends:\n      base     >=4.14 && <4.20\n    , nonempty-vector\n    , QuickCheck\n    , tasty\n    , tasty-quickcheck\n    , vector\n\n  hs-source-dirs:    test\n  ghc-options:       -Wall -threaded\n";
    }
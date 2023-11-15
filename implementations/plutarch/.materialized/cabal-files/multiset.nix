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
      specVersion = "1.10";
      identifier = { name = "multiset"; version = "0.3.4.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "twanvl@gmail.com";
      author = "Twan van Laarhoven";
      homepage = "";
      url = "";
      synopsis = "The Data.MultiSet container type";
      description = "A variation of Data.Set.\nMultisets, sometimes also called bags, can contain multiple copies of the same key.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ];
        buildable = true;
        };
      tests = {
        "doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = if compiler.isGhc && (compiler.version).lt "8.0"
            then false
            else true;
          };
        "multiset-properties" = {
          depends = [
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."checkers" or (errorHandler.buildDepError "checkers"))
            (hsPkgs."multiset" or (errorHandler.buildDepError "multiset"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/multiset-0.3.4.3.tar.gz";
      sha256 = "79fcae15a5d3ce28f0b973ad90290f7451396e81cc92007456ce2bb49b9415c4";
      });
    }) // {
    package-description-override = "name:             multiset\nversion:          0.3.4.3\nauthor:           Twan van Laarhoven\nmaintainer:       twanvl@gmail.com\nbug-reports:      https://github.com/twanvl/multiset/issues\ncategory:         Data Structures\nsynopsis:         The Data.MultiSet container type\ndescription:\n    A variation of Data.Set.\n    Multisets, sometimes also called bags, can contain multiple copies of the same key.\nlicense:          BSD3\nlicense-file:     LICENSE\nbuild-type:       Simple\nCabal-version:    >= 1.10\nextra-source-files: include/Typeable.h CHANGELOG\ntested-with: GHC == 8.6.4, GHC == 8.4.4, GHC == 8.2.2, GHC == 8.0.2,\n             GHC == 7.10.3, GHC == 7.8.4, GHC == 7.6.3, GHC == 7.4.2,\n             GHC == 7.2.2, GHC == 7.0.4\n\nsource-repository head\n    type:     git\n    location: http://github.com/twanvl/multiset.git\n\nLibrary\n  default-language:   Haskell2010\n  exposed-modules:    Data.MultiSet, Data.IntMultiSet\n\n  include-dirs:       include\n  default-extensions: CPP\n  ghc-options:        -Wall\n  build-depends:      containers >= 0.5.4, base >= 4 && < 5, deepseq >=1.2 && <1.5\n\ntest-suite doctests\n  default-language:   Haskell2010\n  type:               exitcode-stdio-1.0\n  ghc-options:        -threaded\n  hs-source-dirs:     test\n  main-is:            Main.hs\n  build-depends:      base >= 4 && < 5\n                    , doctest\n  if impl(ghc < 8.0)\n    buildable: False\n\ntest-suite multiset-properties\n  default-language:   Haskell2010\n  type:               exitcode-stdio-1.0\n  ghc-options:        -threaded\n  hs-source-dirs:     test\n  main-is:            multiset-properties.hs\n  build-depends:      QuickCheck\n                    , base\n                    , checkers >= 0.5\n                    , multiset\n                    , tasty\n                    , tasty-quickcheck\n";
    }
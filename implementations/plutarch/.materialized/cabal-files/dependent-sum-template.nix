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
      identifier = { name = "dependent-sum-template"; version = "0.2.0.0"; };
      license = "LicenseRef-PublicDomain";
      copyright = "";
      maintainer = "Obsidian Systems, LLC <maintainer@obsidian.systems>";
      author = "James Cook <mokus@deepbondi.net>";
      homepage = "https://github.com/obsidiansystems/dependent-sum-template";
      url = "";
      synopsis = "Template Haskell code to generate instances of classes in some package";
      description = "Template Haskell code to generate instances of classes in some package, such as 'GEq' and 'GCompare'.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."some" or (errorHandler.buildDepError "some"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          ];
        buildable = if compiler.isGhc && (compiler.version).lt "7.10"
          then false
          else true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."constraints-extras" or (errorHandler.buildDepError "constraints-extras"))
            (hsPkgs."dependent-sum-template" or (errorHandler.buildDepError "dependent-sum-template"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."some" or (errorHandler.buildDepError "some"))
            (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
            ];
          buildable = if compiler.isGhc && (compiler.version).lt "8.0"
            then false
            else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/dependent-sum-template-0.2.0.0.tar.gz";
      sha256 = "c9632d831c33a81863cb2102943cee58ffa0287c1777facc577da0aab93f5a81";
      });
    }) // {
    package-description-override = "name:                   dependent-sum-template\nversion:                0.2.0.0\nstability:              experimental\n\ncabal-version:          >= 1.10\nbuild-type:             Simple\n\nauthor:                 James Cook <mokus@deepbondi.net>\nmaintainer:             Obsidian Systems, LLC <maintainer@obsidian.systems>\nlicense:                PublicDomain\nhomepage:               https://github.com/obsidiansystems/dependent-sum-template\n\ncategory:               Unclassified\nsynopsis:               Template Haskell code to generate instances of classes in some package\ndescription:            Template Haskell code to generate instances of classes in some package, such as 'GEq' and 'GCompare'.\n\ntested-with:            GHC == 8.4.4,\n                        GHC == 8.6.5,\n                        GHC == 8.8.4,\n                        GHC == 8.10.7,\n                        GHC == 9.0.2,\n                        GHC == 9.6.1\n\nextra-source-files:     ChangeLog.md\n                      , ReadMe.md\n\nsource-repository head\n  type: git\n  location: https://github.com/obsidiansystems/dependent-sum-template\n\nLibrary\n  if impl(ghc < 7.10)\n    buildable: False\n  hs-source-dirs:       src\n  default-language:     Haskell2010\n  exposed-modules:      Data.GADT.Compare.TH\n                        Data.GADT.Show.TH\n  other-modules:        Data.GADT.TH.Internal\n                        Data.GADT.Compare.Monad\n  build-depends:        base >= 3 && <5,\n                        some >= 1.0.1 && < 1.1,\n                        containers >= 0.5.9.2,\n                        mtl,\n                        template-haskell >= 2.11 && < 2.21,\n                        th-abstraction >= 0.4\n\ntest-suite test\n  if impl(ghc < 8.0)\n    buildable: False\n  type: exitcode-stdio-1.0\n  hs-source-dirs: test\n  default-language:     Haskell2010\n  main-is: test.hs\n  build-depends: base\n               , constraints-extras\n               , dependent-sum-template\n               , template-haskell\n               , some\n               , th-abstraction\n";
    }
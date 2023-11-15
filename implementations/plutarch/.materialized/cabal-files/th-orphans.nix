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
      identifier = { name = "th-orphans"; version = "0.13.14"; };
      license = "BSD-3-Clause";
      copyright = "(c) Matt Morrow, Michael Sloan, Ryan Scott";
      maintainer = "Ryan Scott <ryan.gl.scott@gmail.com>";
      author = "Matt Morrow, Michael Sloan, Ryan Scott";
      homepage = "";
      url = "";
      synopsis = "Orphan instances for TH datatypes";
      description = "Orphan instances for TH datatypes.  In particular, instances\nfor Ord and Lift, as well as a few missing Show / Eq.  These\ninstances used to live in haskell-src-meta, and that's where\nthe version number started.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-compat" or (errorHandler.buildDepError "th-compat"))
          (hsPkgs."th-lift" or (errorHandler.buildDepError "th-lift"))
          (hsPkgs."th-reify-many" or (errorHandler.buildDepError "th-reify-many"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ]) ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "7.10") [
          (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
          (hsPkgs."th-lift-instances" or (errorHandler.buildDepError "th-lift-instances"))
          ]) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.2" && (compiler.isGhc && (compiler.version).lt "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."th-lift" or (errorHandler.buildDepError "th-lift"))
            (hsPkgs."th-orphans" or (errorHandler.buildDepError "th-orphans"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/th-orphans-0.13.14.tar.gz";
      sha256 = "9ddb2a1a0f6afeb8b6697256bfa5930f1f75e99624e370931c4b48bd16c3077c";
      });
    }) // {
    package-description-override = "name:               th-orphans\r\nversion:            0.13.14\r\nx-revision: 2\r\ncabal-version:      >= 1.10\r\nbuild-type:         Simple\r\nlicense:            BSD3\r\nlicense-file:       LICENSE\r\ncategory:           Template Haskell\r\nauthor:             Matt Morrow, Michael Sloan, Ryan Scott\r\ncopyright:          (c) Matt Morrow, Michael Sloan, Ryan Scott\r\nmaintainer:         Ryan Scott <ryan.gl.scott@gmail.com>\r\nbug-reports:        https://github.com/mgsloan/th-orphans/issues\r\nstability:          experimental\r\ntested-with:        GHC == 7.0.4\r\n                  , GHC == 7.2.2\r\n                  , GHC == 7.4.2\r\n                  , GHC == 7.6.3\r\n                  , GHC == 7.8.4\r\n                  , GHC == 7.10.3\r\n                  , GHC == 8.0.2\r\n                  , GHC == 8.2.2\r\n                  , GHC == 8.4.4\r\n                  , GHC == 8.6.5\r\n                  , GHC == 8.8.4\r\n                  , GHC == 8.10.7\r\n                  , GHC == 9.0.1\r\n                  , GHC == 9.2.1\r\nsynopsis:           Orphan instances for TH datatypes\r\ndescription:        Orphan instances for TH datatypes.  In particular, instances\r\n                    for Ord and Lift, as well as a few missing Show / Eq.  These\r\n                    instances used to live in haskell-src-meta, and that's where\r\n                    the version number started.\r\nextra-source-files: CHANGELOG.md, README.md\r\n\r\nlibrary\r\n  build-depends:      base >= 4.3 && < 5,\r\n                      template-haskell < 2.22,\r\n                      th-compat >= 0.1 && < 0.2,\r\n                      -- https://github.com/mboes/th-lift/issues/14\r\n                      th-lift >= 0.7.1,\r\n                      th-reify-many >= 0.1.9 && < 0.2,\r\n                      mtl >= 2\r\n\r\n  if !impl(ghc >= 8.0)\r\n    build-depends:    fail == 4.9.*,\r\n                      semigroups >= 0.18.5  && < 0.21\r\n\r\n  -- Use TH to derive Generics instances instead of DeriveGeneric, for < 7.10\r\n  if impl(ghc < 7.10)\r\n    build-depends:    generic-deriving >= 1.9\r\n                    , th-lift-instances\r\n\r\n  -- Prior to GHC 7.6, GHC generics lived in ghc-prim\r\n  if impl(ghc >= 7.2) && impl(ghc < 7.6)\r\n    build-depends:    ghc-prim\r\n\r\n  hs-source-dirs:     src\r\n  ghc-options:        -Wall\r\n  if impl(ghc >= 8.6)\r\n    ghc-options:      -Wno-star-is-type\r\n  exposed-modules:    Language.Haskell.TH.Instances\r\n  other-modules:      Language.Haskell.TH.Instances.Internal\r\n  default-language:   Haskell2010\r\n\r\ntest-suite test\r\n  type:               exitcode-stdio-1.0\r\n  hs-source-dirs:     test\r\n  main-is:            Spec.hs\r\n  other-modules:      TestUtil\r\n  build-depends:      base,\r\n                      bytestring,\r\n                      ghc-prim,\r\n                      hspec,\r\n                      template-haskell,\r\n                      th-lift,\r\n                      th-orphans\r\n  build-tool-depends: hspec-discover:hspec-discover\r\n  default-language:   Haskell2010\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/mgsloan/th-orphans\r\n";
    }
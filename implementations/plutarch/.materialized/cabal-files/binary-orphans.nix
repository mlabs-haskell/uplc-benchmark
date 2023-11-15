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
      specVersion = "1.12";
      identifier = { name = "binary-orphans"; version = "1.0.4.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "";
      url = "";
      synopsis = "Compatibility package for binary; provides instances";
      description = "This package provides instances defined in later versions of @binary@ package\n\nPrior version 1 this packages provided instances for other packages.\nThat functionality is moved to [binary-instances](https://hackage.haskell.org/package/binary-instances) package.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.10")) [
          (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
          (hsPkgs."void" or (errorHandler.buildDepError "void"))
          ]) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          ]) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "9.2")) (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))) ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "8.0" && (compiler.version).lt "9.4")) (hsPkgs."data-array-byte" or (errorHandler.buildDepError "data-array-byte"));
        buildable = true;
        };
      tests = {
        "binary-orphans-test" = {
          depends = (([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."binary-orphans" or (errorHandler.buildDepError "binary-orphans"))
            (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.10")) [
            (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
            (hsPkgs."void" or (errorHandler.buildDepError "void"))
            ]) ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "8.0" && (compiler.version).lt "9.4")) (hsPkgs."data-array-byte" or (errorHandler.buildDepError "data-array-byte"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/binary-orphans-1.0.4.1.tar.gz";
      sha256 = "5f5b789bc86d25bbd054b86b2d9a6a03f4ac0dd480f283564b6d6465cfa2f0d2";
      });
    }) // {
    package-description-override = "cabal-version:      1.12\nname:               binary-orphans\nversion:            1.0.4.1\nx-revision:         3\nsynopsis:           Compatibility package for binary; provides instances\ncategory:           Data, Binary, Parsing, Compatibility\ndescription:\n  This package provides instances defined in later versions of @binary@ package\n  .\n  Prior version 1 this packages provided instances for other packages.\n  That functionality is moved to [binary-instances](https://hackage.haskell.org/package/binary-instances) package.\n\nbuild-type:         Simple\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nlicense:            BSD3\nlicense-file:       LICENSE\ntested-with:\n  GHC ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.8\n   || ==9.4.7\n   || ==9.6.3\n   || ==9.8.1\n\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/binary-orphans.git\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  ghc-options:      -Wall\n  exposed-modules:  Data.Binary.Orphans\n  other-extensions: CPP\n  build-depends:\n      base          >=4.5     && <4.20\n    , binary        >=0.5.1.0 && <0.6 || >=0.7.1.0 && <0.8 || >=0.8.3.0 && <0.8.10\n    , transformers  >=0.3.0.0 && <0.7\n\n  if !impl(ghc >=7.10)\n    build-depends: nats >=1.1.2 && <1.2\n    build-depends: void >=0.7.3 && <0.8\n\n  if !impl(ghc >=8.0)\n    build-depends: fail >=4.9 && <4.10\n    build-depends: semigroups >=0.18.5 && <0.20.1\n\n  if !impl(ghc >=9.2)\n    build-depends: OneTuple >=0.3 && <0.5\n\n  if impl(ghc >=8.0 && <9.4)\n    build-depends: data-array-byte >=0.1.0.1 && <0.2\n\ntest-suite binary-orphans-test\n  default-language: Haskell2010\n  type:             exitcode-stdio-1.0\n  main-is:          Tests.hs\n  hs-source-dirs:   test\n  ghc-options:      -Wall\n  build-depends:\n      base\n    , binary\n    , binary-orphans\n    , OneTuple              >=0.3      && <0.5\n    , QuickCheck            >=2.13.1   && <2.15\n    , quickcheck-instances  >=0.3.28   && <0.4\n    , tagged                >=0.8.6    && <0.9\n    , tasty                 >=0.10.1.2 && <1.6\n    , tasty-quickcheck      >=0.8.3.2  && <0.11\n\n  if !impl(ghc >=8.0)\n    build-depends: semigroups\n\n  if !impl(ghc >=7.10)\n    build-depends:\n        nats\n      , void\n\n  if impl(ghc >=8.0 && <9.4)\n    build-depends: data-array-byte\n";
    }
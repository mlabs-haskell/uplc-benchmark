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
      identifier = { name = "universe-base"; version = "1.1.3.1"; };
      license = "BSD-3-Clause";
      copyright = "2014 Daniel Wagner";
      maintainer = "me@dmwit.com";
      author = "Daniel Wagner";
      homepage = "https://github.com/dmwit/universe";
      url = "";
      synopsis = "A class for finite and recursively enumerable types.";
      description = "A class for finite and recursively enumerable types and some helper functions for enumerating them.\n\n@\nclass Universe a where universe :: [a]\nclass Universe a => Finite a where universeF :: [a]; universeF = universe\n@\n\nThis is slim package definiting only the type-classes and instances\nfor types in GHC boot libraries.\nFor more instances check @universe-instances-*@ packages.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).ge "7.2") ((pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.6") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim")))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.10.3") (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.10.3")) (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.10")) [
          (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
          (hsPkgs."void" or (errorHandler.buildDepError "void"))
          ]) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "9.2")) (if compiler.isGhc && (compiler.version).ge "9.0"
          then [
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            ]
          else [
            (hsPkgs."OneTuple" or (errorHandler.buildDepError "OneTuple"))
            ]);
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."universe-base" or (errorHandler.buildDepError "universe-base"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.10")) (hsPkgs."nats" or (errorHandler.buildDepError "nats"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/universe-base-1.1.3.1.tar.gz";
      sha256 = "a3e7ef305d79c58caa25276929f0cd2c129468484502b93b7f9b3b7b77e5732d";
      });
    }) // {
    package-description-override = "name:               universe-base\nversion:            1.1.3.1\nx-revision:         1\nsynopsis:           A class for finite and recursively enumerable types.\ndescription:\n  A class for finite and recursively enumerable types and some helper functions for enumerating them.\n  .\n  @\n  class Universe a where universe :: [a]\n  class Universe a => Finite a where universeF :: [a]; universeF = universe\n  @\n  .\n  This is slim package definiting only the type-classes and instances\n  for types in GHC boot libraries.\n  For more instances check @universe-instances-*@ packages.\n\nhomepage:           https://github.com/dmwit/universe\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             Daniel Wagner\nmaintainer:         me@dmwit.com\ncopyright:          2014 Daniel Wagner\ncategory:           Data\nbuild-type:         Simple\ncabal-version:      >=1.10\nextra-source-files: changelog\ntested-with:\n  GHC ==7.0.4\n   || ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.8\n   || ==9.4.7\n   || ==9.6.3\n   || ==9.8.1\n\nsource-repository head\n  type:     git\n  location: https://github.com/dmwit/universe\n\nsource-repository this\n  type:     git\n  location: https://github.com/dmwit/universe\n  tag:      instances-extended-1.1\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  exposed-modules:\n    Data.Universe.Class\n    Data.Universe.Helpers\n\n  if impl(ghc >=7.2)\n    exposed-modules: Data.Universe.Generic\n\n    if impl(ghc <7.6)\n      build-depends: ghc-prim\n\n  other-extensions:\n    BangPatterns\n    CPP\n    GADTs\n    ScopedTypeVariables\n    TypeFamilies\n\n  build-depends:\n      base          >=4.3     && <4.20\n    , containers    >=0.4.0.0 && <0.7\n    , tagged        >=0.8.6.1 && <0.9\n    , transformers  >=0.3.0.0 && <0.7\n\n  if impl(ghc >=7.10.3)\n    build-depends: transformers >=0.4.2.0\n\n  if !impl(ghc >=7.10.3)\n    build-depends: transformers-compat >=0.6.1 && <0.8\n\n  if !impl(ghc >=7.10)\n    build-depends:\n        nats  >=1.1.2 && <1.2\n      , void  >=0.7.3 && <0.8\n\n  if !impl(ghc >=8.0)\n    build-depends: semigroups >=0.18.5 && <0.21\n\n  if impl(ghc >=7.4)\n    cpp-options:      -DDEFAULT_SIGNATURES\n    other-extensions: DefaultSignatures\n\n  if !impl(ghc >= 9.2)\n    if impl(ghc >= 9.0)\n      build-depends: ghc-prim\n    else\n      build-depends: OneTuple >=0.3 && <0.5\n\n  if impl(ghc >= 9.0)\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n\ntest-suite tests\n  default-language: Haskell2010\n  other-extensions: ScopedTypeVariables\n  type:             exitcode-stdio-1.0\n  main-is:          Tests.hs\n  hs-source-dirs:   tests\n  ghc-options:      -Wall\n  build-depends:\n      base\n    , containers\n    , QuickCheck     >=2.8.2 && <2.15\n    , universe-base\n\n  if !impl(ghc >=7.10)\n    build-depends: nats\n";
    }
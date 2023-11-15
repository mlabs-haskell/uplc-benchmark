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
      identifier = { name = "parallel"; version = "3.2.2.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "libraries@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Parallel programming library";
      description = "This package provides a library for parallel programming.\n\nFor documentation, start from the \"Control.Parallel.Strategies\"\nmodule below.\n\nFor more tutorial documentation, see the book <https://simonmar.github.io/pages/pcph.html Parallel and Concurrent Programming in Haskell>.\n\nTo understand the principles behind the library, see\n<https://simonmar.github.io/bib/papers/strategies.pdf Seq no more: Better Strategies for Parallel Haskell>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.2.1") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/parallel-3.2.2.0.tar.gz";
      sha256 = "170453a71a2a8b31cca63125533f7771d7debeb639700bdabdd779c34d8a6ef6";
      });
    }) // {
    package-description-override = "cabal-version:  >=1.10\nname:           parallel\nversion:        3.2.2.0\nx-revision:     7\n-- NOTE: Don't forget to update ./changelog.md\nlicense:        BSD3\nlicense-file:   LICENSE\nmaintainer:     libraries@haskell.org\nbug-reports:    https://github.com/haskell/parallel/issues\nsynopsis:       Parallel programming library\ncategory:       Control, Parallelism\nbuild-type:     Simple\n\ntested-with:\n  GHC == 9.8.0\n  GHC == 9.6.3\n  GHC == 9.4.7\n  GHC == 9.2.8\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n  GHC == 7.10.3\n  -- Drop these old GHCs from CI:\n  -- GHC == 7.8.4\n  -- GHC == 7.6.3\n  -- GHC == 7.4.2\n  -- GHC == 7.2.2\n  -- GHC == 7.0.4\n\ndescription:\n    This package provides a library for parallel programming.\n    .\n    For documentation, start from the \"Control.Parallel.Strategies\"\n    module below.\n    .\n    For more tutorial documentation, see the book <https://simonmar.github.io/pages/pcph.html Parallel and Concurrent Programming in Haskell>.\n    .\n    To understand the principles behind the library, see\n    <https://simonmar.github.io/bib/papers/strategies.pdf Seq no more: Better Strategies for Parallel Haskell>.\n\n\nextra-source-files: changelog.md\n\nsource-repository head\n    type:     git\n    location: https://github.com/haskell/parallel.git\n\nlibrary\n    default-language: Haskell2010\n    other-extensions:\n        BangPatterns\n        CPP\n        MagicHash\n        UnboxedTuples\n\n    exposed-modules:\n        Control.Seq\n        Control.Parallel\n        Control.Parallel.Strategies\n\n    build-depends:\n        array      >= 0.3 && < 0.6,\n        base       >= 4.3 && < 4.20,\n        containers >= 0.4 && < 0.8,\n        deepseq    >= 1.1 && < 1.6\n\n    ghc-options: -Wall\n\n    if impl(ghc >= 6.11)\n        -- To improve parallel performance:\n        ghc-options: -feager-blackholing\n\n    if impl(ghc >= 7.2.1)\n        build-depends: ghc-prim\n";
    }
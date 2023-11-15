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
    flags = { template-haskell = true; };
    package = {
      specVersion = "1.18";
      identifier = { name = "recursion-schemes"; version = "5.2.2.5"; };
      license = "BSD-2-Clause";
      copyright = "Copyright (C) 2008-2015 Edward A. Kmett";
      maintainer = "\"Samuel Gélineau\" <gelisam@gmail.com>,\n\"Ryan Scott\" <ryan.gl.scott@gmail.com>,\n\"Luc Tielen\" <luc.tielen@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/recursion-schemes/";
      url = "";
      synopsis = "Representing common recursion patterns as higher-order functions";
      description = "Many recursive functions share the same structure, e.g. pattern-match on the input and, depending on the data constructor, either recur on a smaller input or terminate the recursion with the base case. Another one: start with a seed value, use it to produce the first element of an infinite list, and recur on a modified seed in order to produce the rest of the list. Such a structure is called a recursion scheme. Using higher-order functions to implement those recursion schemes makes your code clearer, faster, and safer. See README for details.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."data-fix" or (errorHandler.buildDepError "data-fix"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.2")) (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.5") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.10") (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "7.10")) [
          (hsPkgs."nats" or (errorHandler.buildDepError "nats"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ]) ++ (pkgs.lib).optionals (flags.template-haskell) [
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          ];
        buildable = true;
        };
      tests = {
        "Expr" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."recursion-schemes" or (errorHandler.buildDepError "recursion-schemes"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.5") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/recursion-schemes-5.2.2.5.tar.gz";
      sha256 = "5cb79bd5d6dd5a0adf61ccc37a93c4fcfaeb6077f60a975c895feb32744d97ec";
      });
    }) // {
    package-description-override = "name:          recursion-schemes\ncategory:      Control, Recursion\nversion:       5.2.2.5\nlicense:       BSD2\ncabal-version: 1.18\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    \"Samuel Gélineau\" <gelisam@gmail.com>,\n               \"Ryan Scott\" <ryan.gl.scott@gmail.com>,\n               \"Luc Tielen\" <luc.tielen@gmail.com>\nstability:     provisional\nhomepage:      http://github.com/ekmett/recursion-schemes/\nbug-reports:   http://github.com/ekmett/recursion-schemes/issues\ncopyright:     Copyright (C) 2008-2015 Edward A. Kmett\nsynopsis:      Representing common recursion patterns as higher-order functions\ndescription:   Many recursive functions share the same structure, e.g. pattern-match on the input and, depending on the data constructor, either recur on a smaller input or terminate the recursion with the base case. Another one: start with a seed value, use it to produce the first element of an infinite list, and recur on a modified seed in order to produce the rest of the list. Such a structure is called a recursion scheme. Using higher-order functions to implement those recursion schemes makes your code clearer, faster, and safer. See README for details.\n\ntested-with:   GHC==7.4.2, GHC==7.6.3, GHC==7.8.4, GHC==7.10.3, GHC==8.0.2, GHC==8.2.2, GHC==8.4.4, GHC==8.6.5, GHC==8.8.4, GHC==8.10.7, GHC==9.0.1, GHC==9.2.1, GHC==9.4.1, GHC==9.6.2, GHC==9.8.1\n\nbuild-type:    Simple\nextra-doc-files: docs/github-compression.png docs/flowchart.svg\nextra-source-files: CHANGELOG.markdown .gitignore README.markdown include/recursion-schemes-common.h\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/recursion-schemes.git\n\nflag template-haskell\n  description: About Template Haskell derivations\n  manual: True\n  default: True\n\nlibrary\n  other-extensions:\n    CPP\n    TypeFamilies\n    Rank2Types\n    FlexibleContexts\n    FlexibleInstances\n    GADTs\n    StandaloneDeriving\n    UndecidableInstances\n\n  hs-source-dirs: src\n  include-dirs: include\n  -- includes: recursion-schemes-common.h\n\n  build-depends:\n    base                 >= 4.5     && < 5,\n    containers           >= 0.4.2.1 && < 0.7,\n    comonad              >= 4       && < 6,\n    data-fix             >= 0.3.0   && < 0.4,\n    free                 >= 4       && < 6,\n    transformers         >= 0.3.0.0 && < 1\n\n  if !impl(ghc >= 8.2)\n    build-depends: bifunctors >= 4 && < 6\n\n  if !impl(ghc >= 8.0)\n    build-depends: semigroups >= 0.10 && < 1\n\n  if impl(ghc < 7.5)\n    build-depends: ghc-prim\n\n  -- Following two conditionals aren't inverses (there are other compilers than GHC)\n  --\n  -- We enforce the fact that with GHC-7.10\n  -- we have at least transformers-0.4.2.0 (the bundled one)\n  -- which has 'Data.Functor.Classes' module. (transformers-0.3 doesn't have)\n  if impl(ghc >= 7.10)\n    build-depends:\n      transformers         >= 0.4.2.0\n\n  if !impl(ghc >= 7.10)\n    build-depends:\n      nats,\n      transformers-compat  >= 0.3     && < 1\n\n  -- Foldable module is first, so cabal repl loads it!\n  exposed-modules:\n    Data.Functor.Foldable\n    Data.Functor.Base\n\n  if flag(template-haskell)\n    build-depends:\n      template-haskell >= 2.5.0.0 && < 2.22,\n      th-abstraction   >= 0.4     && < 0.7\n    exposed-modules:\n      Data.Functor.Foldable.TH\n\n    other-modules:\n      Paths_recursion_schemes\n\n  ghc-options: -Wall\n  if impl(ghc >= 8.6)\n    ghc-options: -Wno-star-is-type\n  default-language: Haskell2010\n\ntest-suite Expr\n  type: exitcode-stdio-1.0\n  main-is: Expr.hs\n  hs-source-dirs: examples\n  ghc-options: -Wall -threaded\n  default-language: Haskell2010\n  build-depends:\n    base,\n    HUnit <1.7,\n    recursion-schemes,\n    template-haskell,\n    transformers     >= 0.2     && < 1\n  if impl(ghc < 7.5)\n    build-depends: ghc-prim\n";
    }
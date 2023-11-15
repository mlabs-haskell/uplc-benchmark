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
    flags = {
      boundschecks = true;
      unsafechecks = false;
      internalchecks = false;
      bench = true;
      properties = true;
      llvm = false;
      };
    package = {
      specVersion = "1.10";
      identifier = {
        name = "vector-algorithms";
        version = "0.9.0.1.0.0.0.0.1";
        };
      license = "BSD-3-Clause";
      copyright = "(c) 2008,2009,2010,2011,2012,2013,2014,2015 Dan Doel\n(c) 2015 Tim Baumann";
      maintainer = "Dan Doel <dan.doel@gmail.com>\nErik de Castro Lopo <erikd@mega-nerd.com>";
      author = "Dan Doel";
      homepage = "https://github.com/erikd/vector-algorithms/";
      url = "";
      synopsis = "Efficient algorithms for vector arrays";
      description = "Efficient algorithms for sorting vector arrays. At some stage\nother vector algorithms may be added.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bitvec" or (errorHandler.buildDepError "bitvec"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.8")) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"));
        buildable = true;
        };
      tests = {
        "properties" = {
          depends = (pkgs.lib).optionals (!(!flags.properties)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
            ];
          buildable = if !flags.properties then false else true;
          };
        };
      benchmarks = {
        "simple-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mwc-random" or (errorHandler.buildDepError "mwc-random"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."vector-algorithms" or (errorHandler.buildDepError "vector-algorithms"))
            ];
          buildable = if !flags.bench then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "https://input-output-hk.github.io/cardano-haskell-packages/package/vector-algorithms-0.9.0.1.0.0.0.0.1.tar.gz";
      sha256 = "7ce4a893273b1d064fe8bec24dd1139fa405174c7417a723d3c49c36e34cccc0";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.10\nname:               vector-algorithms\nversion:            0.9.0.1.0.0.0.0.1\nlicense:            BSD3\nlicense-file:       LICENSE\ncopyright:\n    (c) 2008,2009,2010,2011,2012,2013,2014,2015 Dan Doel\n    (c) 2015 Tim Baumann\n\nmaintainer:\n    Dan Doel <dan.doel@gmail.com>\n    Erik de Castro Lopo <erikd@mega-nerd.com>\n\nauthor:             Dan Doel\nhomepage:           https://github.com/erikd/vector-algorithms/\nsynopsis:           Efficient algorithms for vector arrays\ndescription:\n    Efficient algorithms for sorting vector arrays. At some stage\n    other vector algorithms may be added.\n\ncategory:           Data\nbuild-type:         Simple\nextra-source-files: CHANGELOG.md\n\nsource-repository head\n    type:     git\n    location: https://github.com/erikd/vector-algorithms/\n\nflag boundschecks\n    description: Enable bounds checking\n\nflag unsafechecks\n    description:\n        Enable bounds checking in unsafe operations at the cost of a\n        significant performance penalty.\n\n    default:     False\n\nflag internalchecks\n    description:\n        Enable internal consistency checks at the cost of a\n        significant performance penalty.\n\n    default:     False\n\nflag bench\n    description:\n        Build a benchmarking program to test vector-algorithms\n        performance\n\nflag properties\n    description: Enable the quickcheck tests\n\nflag llvm\n    description: Build using llvm\n    default:     False\n\nlibrary\n    exposed-modules:\n        Data.Vector.Algorithms\n        Data.Vector.Algorithms.Optimal\n        Data.Vector.Algorithms.Insertion\n        Data.Vector.Algorithms.Intro\n        Data.Vector.Algorithms.Merge\n        Data.Vector.Algorithms.Radix\n        Data.Vector.Algorithms.Search\n        Data.Vector.Algorithms.Heap\n        Data.Vector.Algorithms.AmericanFlag\n        Data.Vector.Algorithms.Tim\n\n    hs-source-dirs:   src\n    other-modules:    Data.Vector.Algorithms.Common\n    default-language: Haskell2010\n    include-dirs:     include\n    install-includes: vector.h\n    ghc-options:      -funbox-strict-fields\n    build-depends:\n        base >=4.5 && <5,\n        bitvec >=1.0 && <1.2,\n        vector >=0.6 && <0.14,\n        primitive >=0.3 && <0.9,\n        bytestring >=0.9 && <1.0\n\n    if !impl(ghc >=7.8)\n        build-depends: tagged >=0.4 && <0.9\n\n    if flag(llvm)\n        ghc-options: -fllvm\n\n    if flag(boundschecks)\n        cpp-options: -DVECTOR_BOUNDS_CHECKS\n\n    if flag(unsafechecks)\n        cpp-options: -DVECTOR_UNSAFE_CHECKS\n\n    if flag(internalchecks)\n        cpp-options: -DVECTOR_INTERNAL_CHECKS\n\ntest-suite properties\n    type:             exitcode-stdio-1.0\n    main-is:          Tests.hs\n    hs-source-dirs:   tests/properties\n    other-modules:\n        Optimal\n        Properties\n        Util\n\n    default-language: Haskell2010\n\n    if !flag(properties)\n        buildable: False\n\n    else\n        build-depends:\n            base,\n            bytestring,\n            containers,\n            QuickCheck >2.9 && <2.15,\n            vector,\n            vector-algorithms\n\n    if flag(llvm)\n        ghc-options: -fllvm\n\nbenchmark simple-bench\n    type:             exitcode-stdio-1.0\n    main-is:          Main.hs\n    hs-source-dirs:   bench/simple\n    other-modules:    Blocks\n    default-language: Haskell2010\n    ghc-options:      -Wall\n    build-depends:\n        base,\n        mwc-random,\n        vector,\n        vector-algorithms\n\n    if !flag(bench)\n        buildable: False\n\n    if flag(llvm)\n        ghc-options: -fllvm\n";
    }
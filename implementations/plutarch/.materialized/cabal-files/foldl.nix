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
      identifier = { name = "foldl"; version = "1.4.15"; };
      license = "BSD-3-Clause";
      copyright = "2013 Gabriella Gonzalez";
      maintainer = "GenuineGabriella@gmail.com";
      author = "Gabriella Gonzalez";
      homepage = "";
      url = "";
      synopsis = "Composable, streaming, and efficient left folds";
      description = "This library provides strict left folds that stream in constant\nmemory, and you can combine folds using @Applicative@ style to derive new\nfolds.  Derived folds still traverse the container just once and are often as\nefficient as hand-written folds.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "Foldl" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
            ];
          buildable = true;
          };
        "Scanl" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."foldl" or (errorHandler.buildDepError "foldl"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/foldl-1.4.15.tar.gz";
      sha256 = "9cb2f105141788cbc6e66480a8f46c13d52666899eec1e2cc3dc4d60f606c0ae";
      });
    }) // {
    package-description-override = "Name: foldl\nVersion: 1.4.15\nCabal-Version: >=1.10\nBuild-Type: Simple\nLicense: BSD3\nLicense-File: LICENSE\nCopyright: 2013 Gabriella Gonzalez\nAuthor: Gabriella Gonzalez\nMaintainer: GenuineGabriella@gmail.com\nBug-Reports: https://github.com/Gabriella439/Haskell-Foldl-Library/issues\nSynopsis: Composable, streaming, and efficient left folds\nDescription: This library provides strict left folds that stream in constant\n  memory, and you can combine folds using @Applicative@ style to derive new\n  folds.  Derived folds still traverse the container just once and are often as\n  efficient as hand-written folds.\nCategory: Control\nExtra-Source-Files:\n    CHANGELOG.md\n    README.md\nSource-Repository head\n    Type: git\n    Location: https://github.com/Gabriella439/Haskell-Foldl-Library\n\nLibrary\n    HS-Source-Dirs: src\n    Build-Depends:\n        base         >= 4.11.0.0 && < 5   ,\n        bytestring   >= 0.9.2.1  && < 0.12,\n        random       >= 1.2      && < 1.3 ,\n        primitive                   < 0.9 ,\n        text         >= 0.11.2.0 && < 2.1 ,\n        transformers >= 0.2.0.0  && < 0.7 ,\n        vector       >= 0.7      && < 0.14,\n        containers   >= 0.5.0.0  && < 0.7 ,\n        unordered-containers        < 0.3 ,\n        hashable                    < 1.5 ,\n        contravariant               < 1.6 ,\n        profunctors  >= 4.3.2    && < 5.7 ,\n        semigroupoids >= 1.0     && < 6.1 ,\n        comonad      >= 4.0      && < 6\n    if impl(ghc < 8.0)\n        Build-Depends:\n            semigroups   >= 0.17 && < 1.20\n    Exposed-Modules:\n        Control.Foldl,\n        Control.Foldl.ByteString,\n        Control.Foldl.NonEmpty\n        Control.Foldl.Text,\n        Control.Scanl\n    Other-Modules:\n        Control.Foldl.Optics\n        Control.Foldl.Internal\n        Control.Foldl.Util.Vector\n        Control.Foldl.Util.MVector\n    GHC-Options: -O2 -Wall\n    Default-Language: Haskell2010\n\nBenchmark Foldl\n    Type: exitcode-stdio-1.0\n    HS-Source-Dirs: bench\n    Main-Is: Foldl.hs\n    Build-Depends:\n        base,\n        criterion,\n        foldl\n    GHC-Options: -O2 -Wall -rtsopts -with-rtsopts=-T\n    Default-Language: Haskell2010\n\nBenchmark Scanl\n    Type: exitcode-stdio-1.0\n    HS-Source-Dirs: bench\n    Main-Is: Scanl.hs\n    Build-Depends:\n        base,\n        criterion,\n        foldl\n    GHC-Options: -O2 -Wall -rtsopts -with-rtsopts=-T\n    Default-Language: Haskell2010\n\nTest-Suite doctest\n    Type: exitcode-stdio-1.0\n    HS-Source-Dirs: test\n    Main-Is: doctest.hs\n    Build-Depends:\n        base,\n        doctest >= 0.16\n    GHC-Options: -threaded\n    Default-Language: Haskell2010\n";
    }
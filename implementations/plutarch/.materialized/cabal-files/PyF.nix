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
    flags = { python_test = false; };
    package = {
      specVersion = "2.4";
      identifier = { name = "PyF"; version = "0.11.2.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "guillaum.bouchard@gmail.com";
      author = "Guillaume Bouchard";
      homepage = "";
      url = "";
      synopsis = "Quasiquotations for a python like interpolated string formatter";
      description = "Quasiquotations for a python like interpolated string formatter.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "9.2.1") (hsPkgs."ghc-boot" or (errorHandler.buildDepError "ghc-boot"));
        buildable = true;
        };
      tests = {
        "pyf-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."PyF" or (errorHandler.buildDepError "PyF"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ] ++ (pkgs.lib).optional (flags.python_test) (hsPkgs."process" or (errorHandler.buildDepError "process"));
          buildable = true;
          };
        "pyf-overloaded" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."PyF" or (errorHandler.buildDepError "PyF"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ];
          buildable = true;
          };
        "pyf-failure" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."PyF" or (errorHandler.buildDepError "PyF"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/PyF-0.11.2.1.tar.gz";
      sha256 = "e865c45a9323fc60f0a35f0aedf9e9f5c12258a6344bbd49031c09211b82220a";
      });
    }) // {
    package-description-override = "cabal-version:       2.4\nname:                PyF\nversion:             0.11.2.1\nsynopsis:            Quasiquotations for a python like interpolated string formatter\ndescription:         Quasiquotations for a python like interpolated string formatter.\nlicense:             BSD-3-Clause\nlicense-file:        LICENSE\nauthor:              Guillaume Bouchard\nmaintainer:          guillaum.bouchard@gmail.com\ncategory:            Text\nbuild-type:          Simple\nextra-source-files:  ChangeLog.md Readme.md test/golden/*.golden test/golden96/*.golden\n\nFlag python_test\n    Description: Enable extensive python testing\n    Manual: True\n    Default: False\n\nlibrary\n  exposed-modules:\n                  PyF\n                  PyF.Class\n                  PyF.Internal.PythonSyntax\n                  PyF.Internal.Meta\n                  PyF.Internal.QQ\n                  PyF.Formatters\n                  PyF.Internal.ParserEx\n                  PyF.Internal.Parser\n\n  build-depends:       base >= 4.12 && < 4.20\n                     , bytestring >= 0.10.8 && < 0.13\n                     , template-haskell >= 2.14.0 && < 2.22\n                     , text >= 1.2.3 && < 2.2\n                     , time >= 1.8.0 && < 1.14\n                     , parsec >= 3.1.13 && < 3.2\n                     , mtl >= 2.2.2 && < 2.4\n                     , ghc >= 8.6.1\n  if impl(ghc < 9.2.1)\n    build-depends:\n                       ghc-boot >= 8.6.1 && < 9.7\n  hs-source-dirs: src\n  ghc-options: -Wall -Wunused-packages -Wincomplete-uni-patterns\n  default-language:    Haskell2010\n  default-extensions: QuasiQuotes\n\ntest-suite pyf-test\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             Spec.hs\n  other-modules: SpecUtils SpecCustomDelimiters\n  build-depends:       base, PyF, hspec, template-haskell, text, bytestring, time\n  ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N -Wunused-packages\n  default-language:    Haskell2010\n  if flag(python_test)\n    cpp-options: -DPYTHON_TEST\n    build-depends:       process\n\ntest-suite pyf-overloaded\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             SpecOverloaded.hs\n  build-depends:       base, PyF, hspec, text, bytestring\n  ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N -Wunused-packages\n  default-language:    Haskell2010\n\ntest-suite pyf-failure\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  main-is:             SpecFail.hs\n  build-depends:       base, hspec, text, process, hspec, temporary, filepath, deepseq, HUnit, PyF\n  ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N -Wunused-packages\n  default-language:    Haskell2010\n\nsource-repository head\n  type:     git\n  location: http://github.com/guibou/PyF\n";
    }
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
      identifier = { name = "doctest"; version = "0.22.2"; };
      license = "MIT";
      copyright = "(c) 2009-2023 Simon Hengel";
      maintainer = "Simon Hengel <sol@typeful.net>";
      author = "Simon Hengel <sol@typeful.net>";
      homepage = "https://github.com/sol/doctest#readme";
      url = "";
      synopsis = "Test interactive Haskell examples";
      description = "`doctest` is a tool that checks [examples](https://www.haskell.org/haddock/doc/html/ch03s08.html#idm140354810775744)\nand [properties](https://www.haskell.org/haddock/doc/html/ch03s08.html#idm140354810771856)\nin Haddock comments.\nIt is similar in spirit to the [popular Python module with the same name](https://docs.python.org/3/library/doctest.html).\n\nDocumentation is at <https://github.com/sol/doctest#readme>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."code-page" or (errorHandler.buildDepError "code-page"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
          (hsPkgs."ghc-paths" or (errorHandler.buildDepError "ghc-paths"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      exes = {
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."code-page" or (errorHandler.buildDepError "code-page"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."ghc-paths" or (errorHandler.buildDepError "ghc-paths"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."code-page" or (errorHandler.buildDepError "code-page"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."ghc" or (errorHandler.buildDepError "ghc"))
            (hsPkgs."ghc-paths" or (errorHandler.buildDepError "ghc-paths"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-core" or (errorHandler.buildDepError "hspec-core"))
            (hsPkgs."mockery" or (errorHandler.buildDepError "mockery"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."setenv" or (errorHandler.buildDepError "setenv"))
            (hsPkgs."silently" or (errorHandler.buildDepError "silently"))
            (hsPkgs."stringbuilder" or (errorHandler.buildDepError "stringbuilder"))
            (hsPkgs."syb" or (errorHandler.buildDepError "syb"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
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
      url = "http://hackage.haskell.org/package/doctest-0.22.2.tar.gz";
      sha256 = "afb839c14019c17e3ec7900871a9fc104226028858c724932d53225ae382c6e5";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.36.0.\n--\n-- see: https://github.com/sol/hpack\n\nname:           doctest\nversion:        0.22.2\nsynopsis:       Test interactive Haskell examples\ndescription:    `doctest` is a tool that checks [examples](https://www.haskell.org/haddock/doc/html/ch03s08.html#idm140354810775744)\n                and [properties](https://www.haskell.org/haddock/doc/html/ch03s08.html#idm140354810771856)\n                in Haddock comments.\n                It is similar in spirit to the [popular Python module with the same name](https://docs.python.org/3/library/doctest.html).\n                .\n                Documentation is at <https://github.com/sol/doctest#readme>.\ncategory:       Testing\nbug-reports:    https://github.com/sol/doctest/issues\nhomepage:       https://github.com/sol/doctest#readme\nlicense:        MIT\nlicense-file:   LICENSE\ncopyright:      (c) 2009-2023 Simon Hengel\nauthor:         Simon Hengel <sol@typeful.net>\nmaintainer:     Simon Hengel <sol@typeful.net>\nbuild-type:     Simple\nextra-source-files:\n    example/example.cabal\n    example/src/Example.hs\n    example/test/doctests.hs\n    test/parse/multiple-examples/Foo.hs\n    test/parse/no-examples/Fib.hs\n    test/parse/non-exported/Fib.hs\n    test/parse/property/Fib.hs\n    test/parse/setup-empty/Foo.hs\n    test/parse/setup-only/Foo.hs\n    test/parse/simple/Fib.hs\n    test/extract/argument-list/Foo.hs\n    test/extract/comment-order/Foo.hs\n    test/extract/declaration/Foo.hs\n    test/extract/dos-line-endings/Foo.hs\n    test/extract/export-list/Foo.hs\n    test/extract/imported-module/Bar.hs\n    test/extract/imported-module/Baz.hs\n    test/extract/module-header/Foo.hs\n    test/extract/named-chunks/Foo.hs\n    test/extract/regression/Fixity.hs\n    test/extract/regression/ForeignImport.hs\n    test/extract/regression/ParallelListComp.hs\n    test/extract/regression/ParallelListCompClass.hs\n    test/extract/regression/RewriteRules.hs\n    test/extract/regression/RewriteRulesWithSigs.hs\n    test/extract/setup/Foo.hs\n    test/extract/th/Bar.hs\n    test/extract/th/Foo.hs\n    test/extract/type-class-args/Foo.hs\n    test/extract/type-class/Foo.hs\n    test/extract/type-families/Foo.hs\n    test/integration/bugfixImportHierarchical/ModuleA.hs\n    test/integration/bugfixImportHierarchical/ModuleB.hs\n    test/integration/bugfixMultipleModules/ModuleA.hs\n    test/integration/bugfixMultipleModules/ModuleB.hs\n    test/integration/bugfixOutputToStdErr/Fib.hs\n    test/integration/bugfixWorkingDirectory/description\n    test/integration/bugfixWorkingDirectory/examples/Fib.hs\n    test/integration/bugfixWorkingDirectory/Fib.hs\n    test/integration/color/Foo.hs\n    test/integration/custom-package-conf/Bar.hs\n    test/integration/custom-package-conf/foo/doctest-foo.cabal\n    test/integration/custom-package-conf/foo/Foo.hs\n    test/integration/dos-line-endings/Fib.hs\n    test/integration/failing-multiple/Foo.hs\n    test/integration/failing/Foo.hs\n    test/integration/it/Foo.hs\n    test/integration/it/Setup.hs\n    test/integration/local-stderr-binding/A.hs\n    test/integration/multiline/Multiline.hs\n    test/integration/parse-error/Foo.hs\n    test/integration/property-bool-with-type-signature/Foo.hs\n    test/integration/property-bool/Foo.hs\n    test/integration/property-failing/Foo.hs\n    test/integration/property-implicitly-quantified/Foo.hs\n    test/integration/property-quantified/Foo.hs\n    test/integration/property-setup/Foo.hs\n    test/integration/setup-skip-on-failure/Foo.hs\n    test/integration/setup/Foo.hs\n    test/integration/system-io-imported/A.hs\n    test/integration/template-haskell-bugfix/Main.hs\n    test/integration/template-haskell-bugfix/Printf.hs\n    test/integration/template-haskell/Foo.hs\n    test/integration/test-options/Foo.hs\n    test/integration/testBlankline/Fib.hs\n    test/integration/testCombinedExample/Fib.hs\n    test/integration/testCommentLocation/Foo.hs\n    test/integration/testCPP/Foo.hs\n    test/integration/testDocumentationForArguments/Fib.hs\n    test/integration/testFailOnMultiline/Fib.hs\n    test/integration/testImport/ModuleA.hs\n    test/integration/testImport/ModuleB.hs\n    test/integration/testPutStr/Fib.hs\n    test/integration/testSimple/Fib.hs\n    test/integration/trailing-whitespace/Foo.hs\n    test/integration/with-cbits/Bar.hs\n    test/integration/with-cbits/foo.c\n    CHANGES.markdown\n    README.md\n\nsource-repository head\n  type: git\n  location: https://github.com/sol/doctest\n\nlibrary\n  ghc-options: -Wall\n  hs-source-dirs:\n      src\n  default-extensions:\n      NamedFieldPuns\n      RecordWildCards\n      DeriveFunctor\n      NoImplicitPrelude\n  exposed-modules:\n      Test.DocTest\n      Test.DocTest.Internal.Extract\n      Test.DocTest.Internal.Location\n      Test.DocTest.Internal.Parse\n      Test.DocTest.Internal.Run\n  other-modules:\n      Extract\n      GhcUtil\n      Imports\n      Info\n      Interpreter\n      Language.Haskell.GhciWrapper\n      Location\n      Options\n      PackageDBs\n      Parse\n      Property\n      Run\n      Runner\n      Runner.Example\n      Util\n      Paths_doctest\n  build-depends:\n      base >=4.5 && <5\n    , code-page >=0.1\n    , deepseq\n    , directory\n    , exceptions\n    , filepath\n    , ghc >=8.0 && <9.10\n    , ghc-paths >=0.1.0.9\n    , process\n    , syb >=0.3\n    , transformers\n  default-language: Haskell2010\n  if impl(ghc >= 9.8)\n    ghc-options: -fno-warn-x-partial\n\nexecutable doctest\n  main-is: Main.hs\n  other-modules:\n      Paths_doctest\n  ghc-options: -Wall -threaded\n  hs-source-dirs:\n      driver\n  default-extensions:\n      NamedFieldPuns\n      RecordWildCards\n      DeriveFunctor\n      NoImplicitPrelude\n  build-depends:\n      base >=4.5 && <5\n    , code-page >=0.1\n    , deepseq\n    , directory\n    , doctest\n    , exceptions\n    , filepath\n    , ghc >=8.0 && <9.10\n    , ghc-paths >=0.1.0.9\n    , process\n    , syb >=0.3\n    , transformers\n  default-language: Haskell2010\n  if impl(ghc >= 9.8)\n    ghc-options: -fno-warn-x-partial\n\ntest-suite spec\n  main-is: Spec.hs\n  other-modules:\n      ExtractSpec\n      InfoSpec\n      InterpreterSpec\n      Language.Haskell.GhciWrapperSpec\n      LocationSpec\n      MainSpec\n      OptionsSpec\n      PackageDBsSpec\n      ParseSpec\n      PropertySpec\n      Runner.ExampleSpec\n      RunnerSpec\n      RunSpec\n      UtilSpec\n      Extract\n      GhcUtil\n      Imports\n      Info\n      Interpreter\n      Language.Haskell.GhciWrapper\n      Location\n      Options\n      PackageDBs\n      Parse\n      Property\n      Run\n      Runner\n      Runner.Example\n      Test.DocTest\n      Test.DocTest.Internal.Extract\n      Test.DocTest.Internal.Location\n      Test.DocTest.Internal.Parse\n      Test.DocTest.Internal.Run\n      Util\n      Paths_doctest\n  type: exitcode-stdio-1.0\n  ghc-options: -Wall -threaded\n  cpp-options: -DTEST\n  hs-source-dirs:\n      test\n      src\n  default-extensions:\n      NamedFieldPuns\n      RecordWildCards\n      DeriveFunctor\n      NoImplicitPrelude\n  c-sources:\n      test/integration/with-cbits/foo.c\n  build-tool-depends:\n      hspec-discover:hspec-discover\n  build-depends:\n      HUnit\n    , QuickCheck >=2.13.1\n    , base >=4.5 && <5\n    , code-page >=0.1\n    , deepseq\n    , directory\n    , exceptions\n    , filepath\n    , ghc >=8.0 && <9.10\n    , ghc-paths >=0.1.0.9\n    , hspec >=2.3.0\n    , hspec-core >=2.3.0\n    , mockery\n    , process\n    , setenv\n    , silently >=1.2.4\n    , stringbuilder >=0.4\n    , syb >=0.3\n    , transformers\n  default-language: Haskell2010\n  if impl(ghc >= 9.8)\n    ghc-options: -fno-warn-x-partial\n";
    }
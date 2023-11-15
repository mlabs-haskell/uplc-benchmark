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
    flags = { demos = false; };
    package = {
      specVersion = "3.0";
      identifier = { name = "vty-crossplatform"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2023 Jonathan Daugherty";
      maintainer = "cygnus@foobox.com";
      author = "Jonathan Daugherty";
      homepage = "";
      url = "";
      synopsis = "Cross-platform support for Vty";
      description = "This package provides a generic interface for multiple\nVty platforms in one package so you don't have to\nconditionally depend on them in your cabal file.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
          ] ++ (if system.isOsx
          then [
            (hsPkgs."vty-unix" or (errorHandler.buildDepError "vty-unix"))
            ]
          else if system.isLinux
            then [
              (hsPkgs."vty-unix" or (errorHandler.buildDepError "vty-unix"))
              ]
            else if system.isWindows
              then [
                (hsPkgs."vty-windows" or (errorHandler.buildDepError "vty-windows"))
                ]
              else [
                (hsPkgs."unknown-vty-build-platform" or (errorHandler.buildDepError "unknown-vty-build-platform"))
                ]);
        buildable = true;
        };
      exes = {
        "vty-rogue-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."vty-crossplatform" or (errorHandler.buildDepError "vty-crossplatform"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "vty-event-echo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."vty-crossplatform" or (errorHandler.buildDepError "vty-crossplatform"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "vty-mode-demo" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."vty-crossplatform" or (errorHandler.buildDepError "vty-crossplatform"))
            ];
          buildable = if !flags.demos then false else true;
          };
        "vty-interactive-terminal-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."string-qq" or (errorHandler.buildDepError "string-qq"))
            (hsPkgs."vty" or (errorHandler.buildDepError "vty"))
            (hsPkgs."vty-crossplatform" or (errorHandler.buildDepError "vty-crossplatform"))
            ];
          buildable = if !flags.demos then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/vty-crossplatform-0.1.0.0.tar.gz";
      sha256 = "3c0533fdf9ad65164bb7018dd5e6c9fe1f47be8396bd445bb75a6c6808f999f7";
      });
    }) // {
    package-description-override = "cabal-version:      3.0\r\nname:               vty-crossplatform\r\nversion:            0.1.0.0\r\nx-revision: 1\r\nsynopsis:           Cross-platform support for Vty\r\ndescription:        This package provides a generic interface for multiple\r\n                    Vty platforms in one package so you don't have to\r\n                    conditionally depend on them in your cabal file.\r\nlicense:            BSD-3-Clause\r\nlicense-file:       LICENSE\r\nauthor:             Jonathan Daugherty\r\nmaintainer:         cygnus@foobox.com\r\ncopyright:          (c) 2023 Jonathan Daugherty\r\ncategory:           Graphics\r\nbuild-type:         Simple\r\nextra-doc-files:    CHANGELOG.md\r\n\r\ncommon warnings\r\n    ghc-options: -Wall\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/jtdaugherty/vty-crossplatform.git\r\n\r\nFlag demos\r\n    Description:     Build demonstration programs\r\n    Default:         False\r\n\r\nlibrary\r\n    import:           warnings\r\n    hs-source-dirs:   src\r\n    default-language: Haskell2010\r\n    exposed-modules:  Graphics.Vty.CrossPlatform\r\n                    , Graphics.Vty.CrossPlatform.Testing\r\n    build-depends:    base >= 4.8 && < 5,\r\n                      vty >= 6.0\r\n\r\n    if os(darwin)\r\n        build-depends:  vty-unix\r\n    elif os(linux)\r\n        build-depends:  vty-unix\r\n    elif os(windows)\r\n        build-depends:  vty-windows\r\n    else\r\n        build-depends:  unknown-vty-build-platform\r\n\r\nexecutable vty-rogue-demo\r\n    if !flag(demos)\r\n        Buildable: False\r\n\r\n    hs-source-dirs:      programs\r\n    ghc-options:         -threaded -Wall -Wcompat -O2\r\n    default-language:    Haskell2010\r\n    default-extensions:  CPP\r\n    main-is:             Rogue.hs\r\n    build-depends:       base,\r\n                         vty,\r\n                         vty-crossplatform,\r\n                         random,\r\n                         mtl,\r\n                         array\r\n\r\nexecutable vty-event-echo\r\n    if !flag(demos)\r\n        Buildable: False\r\n\r\n    hs-source-dirs:      programs\r\n    ghc-options:         -threaded -Wall -Wcompat -O2\r\n    default-language:    Haskell2010\r\n    default-extensions:  CPP\r\n    main-is:             EventEcho.hs\r\n    build-depends:       base,\r\n                         vty,\r\n                         vty-crossplatform,\r\n                         containers,\r\n                         mtl\r\n\r\nexecutable vty-mode-demo\r\n    if !flag(demos)\r\n        Buildable: False\r\n\r\n    hs-source-dirs:      programs\r\n    ghc-options:         -threaded -Wall -Wcompat -O2\r\n    default-language:    Haskell2010\r\n    default-extensions:  CPP\r\n    main-is:             ModeDemo.hs\r\n    build-depends:       base,\r\n                         vty,\r\n                         vty-crossplatform\r\n\r\nexecutable vty-interactive-terminal-test\r\n    if !flag(demos)\r\n        Buildable: False\r\n\r\n    hs-source-dirs:      programs\r\n    ghc-options:         -threaded -Wall -Wcompat -O2\r\n    default-language:    Haskell2010\r\n    default-extensions:  CPP\r\n    main-is:             interactive_terminal_test.hs\r\n    build-depends:       base,\r\n                         string-qq,\r\n                         vty,\r\n                         vty-crossplatform\r\n";
    }
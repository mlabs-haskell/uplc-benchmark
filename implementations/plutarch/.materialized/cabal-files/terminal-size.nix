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
      identifier = { name = "terminal-size"; version = "0.3.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "matvey.aksenov@gmail.com";
      author = "Andreas Hammar, Matvey Aksenov";
      homepage = "";
      url = "";
      synopsis = "Get terminal window height and width";
      description = "Get terminal window height and width without ncurses dependency.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && ((compiler.version).ge "7.2" && (compiler.version).lt "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optionals (system.isWindows) [
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.hsc2hs.components.exes.hsc2hs or (pkgs.buildPackages.hsc2hs or (errorHandler.buildToolDepError "hsc2hs:hsc2hs")))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/terminal-size-0.3.4.tar.gz";
      sha256 = "b0f070d6926cdaacf3a412c5518e5c23afca1e0ed00808a5328c96e468b67f49";
      });
    }) // {
    package-description-override = "name:                terminal-size\nversion:             0.3.4\nsynopsis:            Get terminal window height and width\ndescription:\n  Get terminal window height and width without ncurses dependency.\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Andreas Hammar, Matvey Aksenov\nmaintainer:          matvey.aksenov@gmail.com\ncategory:            System\nbuild-type:          Simple\ncabal-version:       >= 1.10\nextra-source-files:\n  README.markdown\n  CHANGELOG.markdown\n\nsource-repository head\n  type:     git\n  location: https://github.com/biegunka/terminal-size\n\nsource-repository this\n  type:     git\n  location: https://github.com/biegunka/terminal-size\n  tag:      0.3.4\n\nlibrary\n  default-language:\n    Haskell2010\n\n  build-depends:\n    base >= 4 && < 5\n  if impl(ghc >= 7.2 && < 7.6)\n     build-depends:\n       ghc-prim\n  if os(windows)\n     build-depends:\n       process,\n       Win32 >= 2.13.2.0 && < 2.14\n\n  build-tools:\n    hsc2hs\n  hs-source-dirs:\n    src\n  exposed-modules:\n    System.Console.Terminal.Size\n\n  other-modules:\n    System.Console.Terminal.Common\n  if os(Windows)\n    other-modules:\n      System.Console.Terminal.Windows\n  else\n    other-modules:\n      System.Console.Terminal.Posix\n\n  ghc-options:\n    -Wall\n    -fno-warn-unused-do-bind\n";
    }
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
    flags = { developer = false; embedded_double_conversion = true; };
    package = {
      specVersion = "2.2";
      identifier = { name = "double-conversion"; version = "2.0.4.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Bryan O'Sullivan <bos@serpentine.com>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/haskell/double-conversion";
      url = "";
      synopsis = "Fast conversion between single and double precision floating point and text";
      description = "A library that performs fast, accurate conversion between\nfloating point and text.\n\nThis library is implemented as bindings to the C++\n@double-conversion@ library written by Florian Loitsch at Google:\n<https://github.com/floitsch/double-conversion>.\n\nNow it can convert single precision numbers, and also it can create\nBuilder, instead of bytestring or text.\n\nThe 'Text' versions of these functions are about 30 times faster\nthan the default 'show' implementation for the 'Double' type.\n\nThe 'ByteString' versions are have very close speed to the 'Text' versions;\n\nBuilder versions (both for Text and Bytestring) are slower on single value,\nbut they are much faster on large number of values\n(up to 20x faster on list with 20000 doubles).\n\nAs a final note, be aware that the @bytestring-show@ package is\nabout 50% slower than simply using 'show'.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "9.4") (hsPkgs."system-cxx-std-lib" or (errorHandler.buildDepError "system-cxx-std-lib"));
        libs = (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "9.4")) (if system.isOsx || system.isFreebsd
          then [ (pkgs."c++" or (errorHandler.sysDepError "c++")) ]
          else if system.isWindows
            then if system.isX86_64 && (compiler.isGhc && (compiler.version).lt "8.6.5")
              then [
                (pkgs."stdc++-6" or (errorHandler.sysDepError "stdc++-6"))
                (pkgs."gcc_s_seh-1" or (errorHandler.sysDepError "gcc_s_seh-1"))
                ]
              else if system.isX86_64
                then [
                  (pkgs."stdc++" or (errorHandler.sysDepError "stdc++"))
                  (pkgs."gcc_s_seh-1" or (errorHandler.sysDepError "gcc_s_seh-1"))
                  ]
                else if compiler.isGhc && (compiler.version).ge "8.6.5"
                  then [
                    (pkgs."stdc++" or (errorHandler.sysDepError "stdc++"))
                    (pkgs."gcc_s_dw2-1" or (errorHandler.sysDepError "gcc_s_dw2-1"))
                    ]
                  else [
                    (pkgs."stdc++-6" or (errorHandler.sysDepError "stdc++-6"))
                    (pkgs."gcc_s_dw2-1" or (errorHandler.sysDepError "gcc_s_dw2-1"))
                    ]
            else [
              (pkgs."stdc++" or (errorHandler.sysDepError "stdc++"))
              ]) ++ (pkgs.lib).optional (!flags.embedded_double_conversion) (pkgs."double-conversion" or (errorHandler.sysDepError "double-conversion"));
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."double-conversion" or (errorHandler.buildDepError "double-conversion"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/double-conversion-2.0.4.2.tar.gz";
      sha256 = "9ab8bc1f0fa7de356c07b23d7d684b6c3ddfa542fd56ea422fb5fd17000aec64";
      });
    }) // {
    package-description-override = "cabal-version:  2.2\r\nname:           double-conversion\r\nversion:        2.0.4.2\r\nx-revision: 2\r\nlicense:        BSD-3-Clause\r\nlicense-file:   LICENSE\r\nhomepage:       https://github.com/haskell/double-conversion\r\nbug-reports:    https://github.com/haskell/double-conversion/issues\r\ncategory:       Text\r\nauthor:         Bryan O'Sullivan <bos@serpentine.com>\r\nmaintainer:     Bryan O'Sullivan <bos@serpentine.com>\r\nstability:      experimental\r\nsynopsis:       Fast conversion between single and double precision floating point and text\r\nbuild-type:     Simple\r\ndescription:\r\n    A library that performs fast, accurate conversion between \r\n    floating point and text.\r\n    .\r\n    This library is implemented as bindings to the C++\r\n    @double-conversion@ library written by Florian Loitsch at Google:\r\n    <https://github.com/floitsch/double-conversion>.\r\n    .\r\n    Now it can convert single precision numbers, and also it can create \r\n    Builder, instead of bytestring or text. \r\n    .\r\n    The 'Text' versions of these functions are about 30 times faster\r\n    than the default 'show' implementation for the 'Double' type.\r\n    .\r\n    The 'ByteString' versions are have very close speed to the 'Text' versions;\r\n    .\r\n    Builder versions (both for Text and Bytestring) are slower on single value,\r\n    but they are much faster on large number of values \r\n    (up to 20x faster on list with 20000 doubles).\r\n    .\r\n    As a final note, be aware that the @bytestring-show@ package is\r\n    about 50% slower than simply using 'show'.\r\n\r\nextra-source-files:\r\n    README.markdown\r\n    benchmarks/*.cabal\r\n    benchmarks/*.hs\r\n    double-conversion/*.cmake.in\r\n    double-conversion/AUTHORS\r\n    double-conversion/CMakeLists.txt\r\n    double-conversion/COPYING\r\n    double-conversion/Changelog\r\n    double-conversion/LICENSE\r\n    double-conversion/Makefile\r\n    double-conversion/README\r\n    double-conversion/SConstruct\r\n    double-conversion/src/*.cc\r\n    double-conversion/src/*.h\r\n    double-conversion/src/CMakeLists.txt\r\n    double-conversion/src/SConscript\r\n    double-conversion/test/CMakeLists.txt\r\n    double-conversion/test/cctest/*.cc\r\n    double-conversion/test/cctest/*.h\r\n    double-conversion/test/cctest/CMakeLists.txt\r\n    double-conversion/test/cctest/SConscript\r\n    include/*.h\r\n    tests/*.hs\r\n\r\nflag developer\r\n  description: operate in developer mode\r\n  default: False\r\n  manual: True\r\n\r\nflag embedded_double_conversion\r\n  description: embed the C++ double_conversion library\r\n  default: True\r\n\r\nlibrary\r\n  if impl(ghc >= 9.4)\r\n    build-depends: system-cxx-std-lib == 1.0\r\n\r\n  elif os(darwin) || os(freebsd)\r\n    extra-libraries: c++\r\n  elif os(windows)\r\n    if arch(x86_64) && impl(ghc < 8.6.5)\r\n      extra-libraries: stdc++-6 gcc_s_seh-1\r\n    elif arch(x86_64)\r\n      extra-libraries: stdc++ gcc_s_seh-1\r\n    elif impl(ghc >= 8.6.5)\r\n      extra-libraries: stdc++ gcc_s_dw2-1\r\n    else \r\n      extra-libraries: stdc++-6 gcc_s_dw2-1\r\n  else\r\n    extra-libraries: stdc++\r\n\r\n  if flag(embedded_double_conversion)\r\n    c-sources:\r\n      cbits/hs-double-conversion-embed.cc\r\n\r\n      double-conversion/src/bignum.cc\r\n      double-conversion/src/bignum-dtoa.cc\r\n      double-conversion/src/cached-powers.cc\r\n      double-conversion/src/diy-fp.cc\r\n      double-conversion/src/double-conversion.cc\r\n      double-conversion/src/fast-dtoa.cc\r\n      double-conversion/src/fixed-dtoa.cc\r\n      double-conversion/src/strtod.cc\r\n    include-dirs: double-conversion/src\r\n  else\r\n    extra-libraries: double-conversion\r\n    c-sources:\r\n      cbits/hs-double-conversion.cc\r\n\r\n  include-dirs:\r\n    include\r\n\r\n  exposed-modules:\r\n    Data.Double.Conversion.Convertable\r\n    Data.Double.Conversion.ByteString\r\n    Data.Double.Conversion.Text\r\n\r\n  other-modules:\r\n    Data.Double.Conversion.Internal.FFI\r\n    Data.Double.Conversion.Internal.ByteString\r\n    Data.Double.Conversion.Internal.ByteStringBuilder\r\n    Data.Double.Conversion.Internal.Text\r\n    Data.Double.Conversion.Internal.TextBuilder\r\n\r\n  build-depends:\r\n    -- Build failure with GHC <= 7.6\r\n    -- https://github.com/haskell/double-conversion/issues/38\r\n    base >= 4.7 && < 5,\r\n    bytestring,\r\n    ghc-prim,\r\n    text >= 0.11.0.8\r\n\r\n  if flag(developer)\r\n    ghc-options: -Werror \r\n    ghc-prof-options: -auto-all\r\n  else\r\n    cc-options: -DNDEBUG \r\n\r\n  ghc-options: -Wall \r\n\r\n  default-language: Haskell2010\r\n\r\ntest-suite tests\r\n  type: exitcode-stdio-1.0\r\n  hs-source-dirs: tests\r\n  main-is: Properties.hs\r\n  other-modules: Regressions\r\n  ghc-options: -Wall\r\n  build-depends:\r\n    HUnit,\r\n    base,\r\n    bytestring,\r\n    double-conversion,\r\n    test-framework,\r\n    test-framework-hunit,\r\n    test-framework-quickcheck2,\r\n    text\r\n  default-language: Haskell2010\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/haskell/double-conversion\r\n";
    }
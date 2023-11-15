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
    flags = { split-these = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "monoidal-containers"; version = "0.6.4.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2014 Ben Gamari";
      maintainer = "ben@smart-cactus.org";
      author = "Ben Gamari";
      homepage = "http://github.com/bgamari/monoidal-containers";
      url = "";
      synopsis = "Containers with monoidal accumulation";
      description = "Containers with merging via monoidal accumulation. The 'Monoid' instances\nprovided by the @containers@ and @unordered-containers@ packages merge\nstructures in a left-biased manner instead of using the underlying monoidal\nstructure of the value.\n\nThis package wraps the types provided by these packages, but provides @Monoid@\ninstances implemented in terms of the value type's 'mappend'. For instance,\nthe @Monoid@ @Map@ instance looks like,\n\n@\ninstance (Ord k, Semigroup a) => Monoid (MonoidalMap k a)\n@";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."newtype" or (errorHandler.buildDepError "newtype"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."witherable" or (errorHandler.buildDepError "witherable"))
          ] ++ (if flags.split-these
          then [
            (hsPkgs."semialign" or (errorHandler.buildDepError "semialign"))
            (hsPkgs."these" or (errorHandler.buildDepError "these"))
            ]
          else [ (hsPkgs."these" or (errorHandler.buildDepError "these")) ]);
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/monoidal-containers-0.6.4.0.tar.gz";
      sha256 = "03019ebf5533dbebf70d5c60bdac52eb2409a8d057a78044f0dabe9df5234f27";
      });
    }) // {
    package-description-override = "name:               monoidal-containers\r\nversion:            0.6.4.0\r\nx-revision: 1\r\nsynopsis:           Containers with monoidal accumulation\r\ndescription:\r\n  Containers with merging via monoidal accumulation. The 'Monoid' instances\r\n  provided by the @containers@ and @unordered-containers@ packages merge\r\n  structures in a left-biased manner instead of using the underlying monoidal\r\n  structure of the value.\r\n  .\r\n  This package wraps the types provided by these packages, but provides @Monoid@\r\n  instances implemented in terms of the value type's 'mappend'. For instance,\r\n  the @Monoid@ @Map@ instance looks like,\r\n  .\r\n  @\r\n  instance (Ord k, Semigroup a) => Monoid (MonoidalMap k a)\r\n  @\r\n\r\nhomepage:           http://github.com/bgamari/monoidal-containers\r\nlicense:            BSD3\r\nlicense-file:       LICENSE\r\nauthor:             Ben Gamari\r\nmaintainer:         ben@smart-cactus.org\r\ncopyright:          (c) 2014 Ben Gamari\r\ncategory:           Data\r\nbuild-type:         Simple\r\ncabal-version:      >=1.10\r\nextra-source-files: Changelog.md\r\ntested-with:\r\n  GHC ==8.2.2 || ==8.4.4 || ==8.6.5 || ==8.8.4 || ==8.10.2 || ==9.0.2 || ==9.2.1 || ==9.4.1\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/bgamari/monoidal-containers\r\n\r\nflag split-these\r\n  description: Use split these/semialign packages\r\n  manual:      False\r\n  default:     True\r\n\r\nlibrary\r\n  exposed-modules:\r\n    Data.HashMap.Monoidal\r\n    Data.IntMap.Monoidal\r\n    Data.IntMap.Monoidal.Strict\r\n    Data.Map.Monoidal\r\n    Data.Map.Monoidal.Strict\r\n\r\n  other-extensions:\r\n    CPP\r\n    DeriveDataTypeable\r\n    DeriveTraversable\r\n    GeneralizedNewtypeDeriving\r\n    MultiParamTypeClasses\r\n\r\n  build-depends:\r\n      aeson                 >=1.0   && <2.3\r\n    , base                  >=4.7   && <4.19\r\n    , containers            >=0.5.9 && <0.7\r\n    , deepseq               >=1.3   && <1.5\r\n    , hashable              >=1.2   && <1.5\r\n    , lens                  >=4.4   && <5.3\r\n    , newtype               >=0.2   && <0.3\r\n    , unordered-containers  >=0.2   && <0.3\r\n    , witherable            >=0.4   && <0.5\r\n\r\n  if flag(split-these)\r\n    build-depends:\r\n        semialign  >=1 && <1.4\r\n      , these      >=1 && <1.3\r\n\r\n  else\r\n    build-depends: these >=0.7 && <0.9\r\n\r\n  hs-source-dirs:   src\r\n  default-language: Haskell2010\r\n";
    }
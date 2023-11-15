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
      specVersion = "3.0";
      identifier = { name = "plutarch"; version = "1.3.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "";
      author = "Las Safin <me@las.rs>";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."constraints" or (errorHandler.buildDepError "constraints"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."flat" or (errorHandler.buildDepError "flat"))
          (hsPkgs."generics-sop" or (errorHandler.buildDepError "generics-sop"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."sop-core" or (errorHandler.buildDepError "sop-core"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://_0/package/plutarch-1.3.0.tar.gz";
      sha256 = "1111111111111111111111111111111111111111111111111111111111111111";
      });
    }) // {
    package-description-override = "cabal-version:      3.0\nname:               plutarch\nversion:            1.3.0\nauthor:             Las Safin <me@las.rs>\nlicense:            MIT\nextra-source-files: README.md\ntested-with:        GHC ==9.2.4\n\ncommon c\n  default-language:   GHC2021\n  default-extensions:\n    NoFlexibleInstances\n    NoMonomorphismRestriction\n    NoStarIsType\n    BlockArguments\n    DataKinds\n    DefaultSignatures\n    DeriveAnyClass\n    DerivingStrategies\n    DerivingVia\n    DisambiguateRecordFields\n    DuplicateRecordFields\n    FunctionalDependencies\n    GADTs\n    ImpredicativeTypes\n    LambdaCase\n    LexicalNegation\n    LiberalTypeSynonyms\n    MonadComprehensions\n    MultiWayIf\n    NegativeLiterals\n    NondecreasingIndentation\n    NumDecimals\n    OverloadedLabels\n    OverloadedLists\n    OverloadedRecordDot\n    OverloadedStrings\n    PackageImports\n    PartialTypeSignatures\n    PatternSynonyms\n    QualifiedDo\n    QuantifiedConstraints\n    QuasiQuotes\n    RankNTypes\n    RecordWildCards\n    RecursiveDo\n    RoleAnnotations\n    TypeFamilies\n    TypeFamilyDependencies\n    UnicodeSyntax\n    UnliftedFFITypes\n    UnliftedNewtypes\n    ViewPatterns\n\n  ghc-options:\n    -Weverything -Wno-unused-do-bind -Wno-missing-kind-signatures\n    -Wno-partial-type-signatures -Werror -Wno-implicit-prelude\n    -Wno-name-shadowing -Wno-unsafe -Wno-missing-safe-haskell-mode\n    -Wno-missing-local-signatures -Wno-prepositive-qualified-module\n    -Wno-missing-import-lists -Wno-monomorphism-restriction\n    -Wno-all-missed-specializations -Wno-unrecognised-warning-flags\n    -Wno-unrecognised-pragmas -fprint-equality-relations\n    -fprint-explicit-kinds -fprint-explicit-foralls\n\nlibrary\n  import:          c\n  exposed-modules:\n    Plutarch\n    Plutarch.Api.Internal.Hashing\n    Plutarch.Api.V1\n    Plutarch.Api.V1.Address\n    Plutarch.Api.V1.AssocMap\n    Plutarch.Api.V1.Contexts\n    Plutarch.Api.V1.Crypto\n    Plutarch.Api.V1.DCert\n    Plutarch.Api.V1.Interval\n    Plutarch.Api.V1.Maybe\n    Plutarch.Api.V1.Scripts\n    Plutarch.Api.V1.Time\n    Plutarch.Api.V1.Tuple\n    Plutarch.Api.V1.Tx\n    Plutarch.Api.V1.Value\n    Plutarch.Api.V2\n    Plutarch.Api.V2.Contexts\n    Plutarch.Api.V2.Tx\n    Plutarch.Bool\n    Plutarch.Builtin\n    Plutarch.ByteString\n    Plutarch.Crypto\n    Plutarch.DataRepr\n    Plutarch.DataRepr.Internal\n    Plutarch.DataRepr.Internal.Field\n    Plutarch.DataRepr.Internal.FromData\n    Plutarch.DataRepr.Internal.HList\n    Plutarch.DataRepr.Internal.HList.Utils\n    Plutarch.Either\n    Plutarch.Evaluate\n    Plutarch.FFI\n    Plutarch.Integer\n    Plutarch.Internal\n    Plutarch.Internal.Evaluate\n    Plutarch.Internal.Generic\n    Plutarch.Internal.Newtype\n    Plutarch.Internal.Other\n    Plutarch.Internal.PLam\n    Plutarch.Internal.PlutusType\n    Plutarch.Internal.PrettyStack\n    Plutarch.Internal.Quantification\n    Plutarch.Internal.ScottEncoding\n    Plutarch.Internal.Trace\n    Plutarch.Internal.TypeFamily\n    Plutarch.Internal.Witness\n    Plutarch.Lift\n    Plutarch.List\n    Plutarch.Maybe\n    Plutarch.Monadic\n    Plutarch.Num\n    Plutarch.Pair\n    Plutarch.Positive\n    Plutarch.Prelude\n    Plutarch.Pretty\n    Plutarch.Pretty.Internal.BuiltinConstant\n    Plutarch.Pretty.Internal.Config\n    Plutarch.Pretty.Internal.Name\n    Plutarch.Pretty.Internal.TermUtils\n    Plutarch.Pretty.Internal.Types\n    Plutarch.Rational\n    Plutarch.Reducible\n    Plutarch.Script\n    Plutarch.Show\n    Plutarch.String\n    Plutarch.TermCont\n    Plutarch.Trace\n    Plutarch.TryFrom\n    Plutarch.Unit\n    Plutarch.Unsafe\n\n  build-depends:\n    , base\n    , bytestring\n    , constraints\n    , containers\n    , cryptonite\n    , data-default\n    , flat\n    , generics-sop\n    , lens\n    , memory\n    , mtl\n    , plutus-core\n    , plutus-ledger-api\n    , plutus-tx\n    , prettyprinter\n    , random\n    , serialise\n    , sop-core\n    , text\n";
    }
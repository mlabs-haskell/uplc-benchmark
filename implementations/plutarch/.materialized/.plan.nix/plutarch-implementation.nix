{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "3.4";
      identifier = { name = "plutarch-implementation"; version = "0.1.0.0"; };
      license = "GPL-3.0-or-later";
      copyright = "";
      maintainer = "t4ccer@gmail.com";
      author = "t4ccer";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "CHANGELOG.md" ];
      };
    components = {
      exes = {
        "plutarch-implementation" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."plutarch" or (errorHandler.buildDepError "plutarch"))
            (hsPkgs."plutus-core" or (errorHandler.buildDepError "plutus-core"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././.; }
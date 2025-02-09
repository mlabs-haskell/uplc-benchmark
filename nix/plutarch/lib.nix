{ fetchFromGitHub
, mkHaskellPackage
}:

let
  plutarchPackage = fetchFromGitHub {
    owner = "Plutonomicon";
    repo = "plutarch-plutus";
    rev = "v1.10.0";
    hash = "sha256-lU2JF9KYvzEPfVLHdLkrM1hTTuc9NYi2hQPFnLDm2d8=";
  };

  cardanoPackages = fetchFromGitHub {
    owner = "IntersectMBO";
    repo = "cardano-haskell-packages";
    rev = "3167b742cea332e1c978d8ecc69ef8d6bd0d6e19"; # branch: repo
    hash = "sha256-oCObuK/TY71lL+vDiRT0/Hhrsq4GRC7n8kcKBeonoUk=";
  };
in

args:
mkHaskellPackage (args // {
  externalRepositories = {
    "https://chap.intersectmbo.org" = cardanoPackages;
  } // (args.externalRepositories or { });

  externalDependencies = [
    "${plutarchPackage}"
  ] ++ (args.externalDependencies or [ ]);
})

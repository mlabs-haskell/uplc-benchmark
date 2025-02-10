{ fetchFromGitHub
, mkHaskellPackage
}:

let
  plutarchPackage = fetchFromGitHub {
    owner = "Plutonomicon";
    repo = "plutarch-plutus";
    rev = "9a7ef03b398d7b1f95e801deb0216ef880d0f76a";
    hash = "sha256-MCoTB5Hukch9HaWvW6q7fpAhnfFOIYaEgW12rjCfcrU=";
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
    "${plutarchPackage}/plutarch-ledger-api"
  ] ++ (args.externalDependencies or [ ]);
})

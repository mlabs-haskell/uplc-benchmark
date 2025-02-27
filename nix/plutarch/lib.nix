{ fetchFromGitHub
, mkHaskellPackage
}:

let
  plutarchPackage = fetchFromGitHub {
    owner = "Plutonomicon";
    repo = "plutarch-plutus";
    rev = "7b346d00596531d3682204e226f7457d51849a21";
    hash = "sha256-k6Sm8DqSvaRDbNsIivzbw/1JrWuiDT7xoVll9ZqGgXM=";
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

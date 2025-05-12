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
    rev = "5f473b88ec38382dd9d05bdf50ecc03a26461fa8"; # branch: repo
    hash = "sha256-Ts/+6Xlh++ePF4VjWIVdvkmnBJR5Vde1c/0KqraUv/8=";
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

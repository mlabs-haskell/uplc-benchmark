{ fetchFromGitHub
, mkHaskellPackage
}:

let
  plutarchPackage = fetchFromGitHub {
    owner = "Plutonomicon";
    repo = "plutarch-plutus";
    rev = "96de13396a469f652b48735c80b8f55b37b8b396"; # branch: staging
    hash = "sha256-NFYBw/QGvsE58fJqlTP2+nFPsWnsI9rJbeHSOEeOhUw=";
  };

  cardanoPackages = fetchFromGitHub {
    owner = "IntersectMBO";
    repo = "cardano-haskell-packages";
    rev = "e062328804c933d296e5956c989b326ea3c69eeb"; # branch: repo
    hash = "sha256-IH5nYTjx+CYAK4zQAkOs475X+AOhP/GPgwXm5LQHsEE=";
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

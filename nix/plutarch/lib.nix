{ fetchFromGitHub
, mkHaskellPackage
}:

let
  plutarchPackage = fetchFromGitHub {
    owner = "Plutonomicon";
    repo = "plutarch-plutus";
    rev = "288d9140468ae98abe1c9a4c0bb1c19a82eb7cd6"; # branch: master
    hash = "sha256-aeaZMW5Y3r5GdSyrfrrKOuGahcL5MVkDUNggunbmtv0=";
  };

  cardanoPackages = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-haskell-packages";
    rev = "3df392af2a61d61bdac1afd9c3674f27d6aa8efc"; # branch: repo
    hash = "sha256-vvm56KzA6jEkG3mvwh1LEdK4H4FKxeoOJNz90H8l8dQ=";
  };
in

args:
mkHaskellPackage (args // {
  externalRepositories = {
    "https://input-output-hk.github.io/cardano-haskell-packages" = cardanoPackages;
  } // (args.externalRepositories or { });

  externalDependencies = [
    "${plutarchPackage}"
    "${plutarchPackage}/plutarch-extra"
  ] ++ (args.externalDependencies or [ ]);
})

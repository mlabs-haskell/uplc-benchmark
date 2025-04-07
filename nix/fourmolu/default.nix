{ mkHaskellPackage
, fetchFromGitHub
}:

(mkHaskellPackage {
  name = "fourmolu";
  src = fetchFromGitHub {
    owner = "fourmolu";
    repo = "fourmolu";
    rev = "v0.17.0.0";
    hash = "sha256-zOdltPAA1q4mc1UHWHab+OSf5zg3cE4QXnBlWlYcelY=";
  };
}).packages."fourmolu:exe:fourmolu"

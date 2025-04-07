{ mkHaskellPackage
, fetchFromGitHub
}:

(mkHaskellPackage {
  name = "fourmolu";
  src = fetchFromGitHub {
    owner = "fourmolu";
    repo = "fourmolu";
    rev = "v0.18.0.0";
    hash = "sha256-VygaYu/sK61TFaKXnsfC+GaXqRccb1Ue/4Ut5vbdpvA=";
  };
}).packages."fourmolu:exe:fourmolu"

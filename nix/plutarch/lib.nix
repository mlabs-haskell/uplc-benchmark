{ stdenv
, fetchFromGitHub
}:

let
  defaultPlutarchPackage = stdenv.mkDerivation (finalArgs: {
    pname = "plutarch-src";
    version = "380df4c8101dd6e0dadc620c1f523f5ae2edbc27"; # branch: master
    src = fetchFromGitHub {
      owner = "Plutonomicon";
      repo = "plutarch-plutus";
      rev = finalArgs.version;
      sha256 = "sha256-jPVA4H3ut8umpzVYWxWjzQZQ6q1l8ikAbW3cZZe29VA=";
    };

    patches = [ ./fix-plutarch.patch ];

    dontConfigure = true;
    dontBuild = true;

    installPhase = ''
      mkdir -p "$out"
      cp -r * "$out"
    '';

    dontFixup = true;
  });

  defaultCardanoPackages = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-haskell-packages";
    rev = "835af81be5bd76342191bd64875dbcbc2c45a39f"; # branch: repo
    hash = "sha256-ZTBmOWmgYg8jVDB3VFu3VSpBaKOGOkp/0u+M9tyTalk=";
  };
in

args:
let
  cardanoPackages =
    if args.cardanoPackages or null == null
    then defaultCardanoPackages
    else args.cardanoPackages;

  plutarchPackage =
    if args.plutarchPackage or null == null
    then defaultPlutarchPackage
    else args.plutarchPackage;

in
{
  inherit (args) src ghcVersion;

  externalRepositories = {
    "https://input-output-hk.github.io/cardano-haskell-packages" = cardanoPackages;
  };

  externalDependencies = [
    plutarchPackage
  ] ++ args.externalDependencies;
}
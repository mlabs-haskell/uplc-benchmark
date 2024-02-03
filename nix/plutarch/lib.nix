{ mkHaskellPackage
, applyPatches
, fetchpatch
, plutarch
, cardanoPackages
}:

let
  plutarchPackage = applyPatches {
    name = "plutarch-patched";
    src = plutarch;

    patches = [
      # https://github.com/Plutonomicon/plutarch-plutus/pull/650
      (fetchpatch {
        url = "https://github.com/Plutonomicon/plutarch-plutus/commit/7256acb8db3230d2453460f0358582283c69da5f.patch";
        hash = "sha256-y/F1ZwLDC5E4vh8F+JTQStHJsQ1ZEe9LIZcwSGMSUek=";
      })
    ];
  };
in

args:
let
  finalArgs = args // {
    externalRepositories = {
      "https://input-output-hk.github.io/cardano-haskell-packages" = cardanoPackages;
    } // (args.externalRepositories or { });

    externalDependencies = [
      "${plutarchPackage}"
      "${plutarchPackage}/plutarch-extra"
    ] ++ (args.externalDependencies or [ ]);
  };
in
mkHaskellPackage finalArgs

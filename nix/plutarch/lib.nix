{ mkHaskellPackage
, plutarch
, cardanoPackages
}:

args:
mkHaskellPackage (args // {
  externalRepositories = {
    "https://chap.intersectmbo.org" = cardanoPackages;
  } // (args.externalRepositories or { });

  externalDependencies = [
    "${plutarch}"
    "${plutarch}/plutarch-ledger-api"
  ] ++ (args.externalDependencies or [ ]);
})

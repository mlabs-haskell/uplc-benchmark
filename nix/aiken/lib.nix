{ stdenv
, writeShellApplication
, python3
, lib
, fetchzip
, yj
, jq
, aiken
}:

let
  mkPythonApplication = name: path: writeShellApplication {
    inherit name;

    runtimeInputs = [
      python3
    ];

    text = "python3 ${path}";
  };

  aiken2nix = mkPythonApplication "aiken2nix" ./aiken2nix.py;
  aiken2bin = mkPythonApplication "aiken2bin" ./aiken2bin.py;

  fetchPackage = { url, hash }: fetchzip {
    inherit url hash;
  };

  vendorPackage = name: p: ''
    cp --no-preserve=all -r ${fetchPackage p} ./build/packages/${name}
  '';

  vendorAikenPackages = lock: ''
    if ! echo '${lock.aiken_lock_hash} aiken.lock' | sha256sum --check; then
      echo "aiken2nix: error: aiken.lock file has been modified. Run aiken2nix again to regenerate the aiken-nix.lock file."
      exit 1
    fi

    mkdir -p ./build/packages
    cat aiken.lock | yj -tj | jq '{packages: .requirements}' | yj -jt > ./build/packages/packages.toml
    ${builtins.concatStringsSep "\n" (lib.mapAttrsToList vendorPackage lock.sources)}
  '';

  mkPackage = args:
    let
      aikenNixLockPath = "${args.src}/aiken-nix.lock";

      aikenNixLock = builtins.fromJSON (
        if builtins.pathExists aikenNixLockPath
        then builtins.readFile aikenNixLockPath
        else throw "aiken2nix: error: ${aikenNixLockPath} file does not exist. Run `aiken2nix` in root of your Aiken project to create it. Make sure to add it to git when using flakes."
      );
    in
    stdenv.mkDerivation (args // {
      nativeBuildInputs = [
        aiken
        aiken2bin
        yj
        jq
      ] ++ (args.nativeBuildInputs or [ ]);

      configurePhase = ''
        runHook preConfigure

        ${vendorAikenPackages aikenNixLock}

        runHook postConfigure
      '';

      buildPhase = ''
        runHook preBuild

        aiken build

        runHook postBuild
      '';

      installPhase = ''
        runHook preInstall

        mkdir -p $out
        aiken2bin

        runHook postInstall
      '';
    });

  preCommitAikenFmt = args: {
    name = "aiken-fmt";
    files = "\\.ak$";
    entry = "${aiken}/bin/aiken fmt";
  } // args;
in
{
  inherit mkPackage;

  packages = {
    inherit aiken aiken2nix aiken2bin;
  };

  preCommit = {
    aikenFmt = preCommitAikenFmt;
  };
}

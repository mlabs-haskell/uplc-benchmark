{ fetchFromGitHub
, rustPlatform
, openssl
, pkg-config
, stdenv
, writeShellApplication
, python3
, lib
, fetchzip
, yj
, jq
}:

let
  aiken = rustPlatform.buildRustPackage rec {
    pname = "aiken";
    version = "1.0.23-alpha";

    src = fetchFromGitHub {
      owner = "aiken-lang";
      repo = "aiken";
      rev = "v${version}";
      hash = "sha256-MmEQVX66zXaK+rOOhaSHQm5kSDIfQTzmWozfsJIzt/M=";
    };

    cargoHash = "sha256-JWid+E5ECouPC/XqlATXb2HY6AQLZOXQnOJsnzzcudU=";

    buildInputs = [
      openssl
    ];

    nativeBuildInputs = [
      pkg-config
    ];
  };

  aiken2nix = writeShellApplication {
    name = "aiken2nix";

    runtimeInputs = [
      python3
    ];

    text = ''
      ${./aiken2nix.py}
    '';
  };

  mkPackage = { aikenLock, ... }@args:
    let
      fetchPackage = { url, hash }: fetchzip {
        inherit url hash;
      };

      vendorPackage = name: p: ''
        cp --no-preserve=all -r ${fetchPackage p} ./build/packages/${name}
      '';
    in
    stdenv.mkDerivation (args // {
      nativeBuildInputs = [
        aiken
        yj
        jq
      ] ++ (args.nativeBuildInputs or [ ]);

      configurePhase = ''
        runHook preConfigure

        mkdir -p ./build/packages
        cat aiken.lock | yj -tj | jq '{packages: .requirements}' | yj -jt > ./build/packages/packages.toml
        ${builtins.concatStringsSep "\n"
          (lib.mapAttrsToList vendorPackage (builtins.fromJSON (builtins.readFile aikenLock)))}
        
        runHook postConfigure
      '';
    });
in
{
  inherit mkPackage;
  inherit aiken aiken2nix;
}

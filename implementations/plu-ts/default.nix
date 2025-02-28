{
  perSystem = { pkgs, lib, config, ... }: {
    devShells.plu-ts-implementation = pkgs.mkShell {
      shellHook = config.pre-commit.installationScript;
      nativeBuildInputs = [
        pkgs.nodejs
      ];
    };

    packages.plu-ts-implementation-compiled = pkgs.buildNpmPackage {
      name = "plu-ts-compiled";
      src = builtins.filterSource (path: _: !(lib.strings.hasSuffix ".nix" path)) ./.;

      npmDepsHash = "sha256-/v/zP6jhKmBjpXIJ9yW1gFFYik89zONAs6CDAHg0c68=";

      npmBuildScript = "start";

      installPhase = ''
        runHook preInstall

        cp -r out $out

        runHook postInstall
      '';

      passthru.versions.plu-ts = (builtins.fromJSON (builtins.readFile ./package-lock.json)).packages."node_modules/@harmoniclabs/plu-ts".version;
    };
  };
}

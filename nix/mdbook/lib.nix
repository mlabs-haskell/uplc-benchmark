{ stdenv
, mdbook
}:

{ name
, src
, preBuild ? ""
}:

stdenv.mkDerivation {
  inherit name src;

  nativeBuildInputs = [
    mdbook
  ];

  buildPhase = ''
    runHook preBuild
    ${preBuild}
    mdbook build . --dest-dir $out
    runHook postBuild
  '';

  dontCheck = true;
  dontInstall = true;
  dontFixup = true;
}

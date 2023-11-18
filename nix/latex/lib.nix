{ stdenv
, texlive
}:

{ name # : string
, src # : path
, mainFile # : string
}:

stdenv.mkDerivation {
  inherit name src;

  nativeBuildInputs = [
    texlive.combined.scheme-full
  ];

  buildPhase = ''
    runHook preBuild
    pdflatex -halt-on-error -interaction=nonstopmode -draftmode "${mainFile}"
    pdflatex -halt-on-error -interaction=nonstopmode -draftmode "${mainFile}"
    if [ -f $(basename "${mainFile}" .tex).bib ]; then
       bibtex $(basename "${mainFile}" .tex)
       pdflatex -halt-on-error -interaction=nonstopmode -draftmode "${mainFile}"
    fi
    pdflatex -halt-on-error -interaction=nonstopmode "${mainFile}"
    runHook postBuild
  '';

  dontCheck = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp *.pdf $out
    runHook postInstall
  '';

  dontFixup = true;
}

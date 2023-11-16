{ stdenv
, writeTextFile
}:

{ name
, src
, files
, cabalBuildDepends ? [ ]
, lbfGen
}:

let
  cabalFile = writeTextFile {
    name = "${name}-cabal";
    text = ''
      cabal-version:      3.0
      name:               ${name}
      version:            0.0.0.1
      synopsis:           A Cabal project that contains LambdaBuffers generated Haskell modules
      build-type:         Simple

      library
          exposed-modules: EXPOSED_MODULES
          autogen-modules: EXPOSED_MODULES
          hs-source-dirs:     autogen
          default-language: Haskell2010
          default-extensions: NoImplicitPrelude
          build-depends: ${builtins.concatStringsSep ", " cabalBuildDepends}
    '';
  };

  lbfBuilt = stdenv.mkDerivation {
    name = "${name}-lb";
    inherit src;

    buildPhase = ''
      cp -r $src/* .
      ${lbfGen}/bin/${lbfGen.meta.mainProgram} ${builtins.concatStringsSep " " files}
    '';

    installPhase = ''
      cp -r autogen $out
    '';
  };
in
stdenv.mkDerivation {
  inherit name src;

  buildPhase = ''
    cp -r ${lbfBuilt} autogen
    cp ${./cabal.project} cabal.project

    EXPOSED_MODULES=$(find autogen -name "*.hs" | while read f; do grep -Eo 'module\s+\S+\s+' $f | head -n 1 | sed -r 's/module\s+//' | sed -r 's/\s+//'; done | tr '\n' ' ')
    echo "Found generated modules $EXPOSED_MODULES"
    cat ${cabalFile} | sed -r "s/EXPOSED_MODULES/$EXPOSED_MODULES/" > ${name}.cabal
  '';

  installPhase = ''
    mkdir -p $out
    cp -r . $out
  '';
}

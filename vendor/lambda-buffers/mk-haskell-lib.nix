{ stdenv
, writeTextFile
, mkLbfCall
}:

{ name
, src
, files
, exposedModules ? [ ]
, cabalBuildDepends ? [ ]
, lbfGen
}:

let
  lbfGenBin = mkLbfCall lbfGen;

  cabalFile = writeTextFile {
    name = "${name}-cabal";
    text = ''
      cabal-version:      3.0
      name:               ${name}
      version:            0.0.0.1
      synopsis:           A Cabal project that contains LambdaBuffers generated Haskell modules
      build-type:         Simple

      library
          exposed-modules: ${builtins.concatStringsSep ", " exposedModules}
          autogen-modules: ${builtins.concatStringsSep ", " exposedModules}
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
      ${lbfGenBin}/bin/${lbfGenBin.meta.mainProgram} ${builtins.concatStringsSep " " files}
    '';

    installPhase = ''
      cp -r autogen $out
    '';
  };
in
stdenv.mkDerivation {
  inherit name src;

  installPhase = ''
    mkdir -p $out
    cp -r ${lbfBuilt} $out/autogen;
    cp ${cabalFile} $out/${name}.cabal
  '';
}

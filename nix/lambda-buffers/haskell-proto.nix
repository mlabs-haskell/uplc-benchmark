# From: https://github.com/mlabs-haskell/protobufs.nix/blob/main/src/haskell-proto.nix
# LICENSE: TODO: Missing file

{ stdenv
, lib
, protobuf
, proto-lens-protoc
, mkCli
}:

{ src
, proto
  # TODO: name / pname + version
, cabalPackageName
, cabalPackageVersion ? "0.1.0.0"
, cabalBuildInputs ? [ ]
}:

let
  depPackageNames = builtins.map (dep: dep.name) cabalBuildInputs;
  cabalTemplate = ''
    cabal-version:      3.0
    name:               ${cabalPackageName}
    version:            ${cabalPackageVersion}
    synopsis:           A Cabal project that contains protoc/proto-lens-protoc generated Haskell modules
    build-type:         Simple

    library
        exposed-modules: EXPOSED_MODULES
        autogen-modules: EXPOSED_MODULES

        hs-source-dirs:     src

        default-language: Haskell2010
        build-depends:
            base,
            proto-lens-runtime,
            ${builtins.concatStringsSep "," depPackageNames}
  '';
in
stdenv.mkDerivation {
  src = builtins.filterSource (path: _: lib.strings.hasSuffix ".proto" path) src;
  name = cabalPackageName;
  buildInputs = [
    protobuf
    proto-lens-protoc
  ];
  buildPhase = ''
    mkdir src
    protoc ${mkCli {
      "plugin=protoc-gen-haskell" = "${proto-lens-protoc}/bin/proto-lens-protoc";
      "proto_path" = "${src}";
      "haskell_out" = "src";
    }} ${src}/${proto}

    EXPOSED_MODULES=$(find src -name "*.hs" | while read f; do grep -Eo 'module\s+\S+\s+' $f | head -n 1 | sed -r 's/module\s+//' | sed -r 's/\s+//'; done | tr '\n' ' ')
    echo "Found generated modules $EXPOSED_MODULES"
    echo '${cabalTemplate}' | sed -r "s/EXPOSED_MODULES/$EXPOSED_MODULES/" > ${cabalPackageName}.cabal
  '';

  installPhase = ''
    cp -r . $out
  '';
}

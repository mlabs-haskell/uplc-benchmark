{
  pkgs = hackage:
    {
      packages = {
        mtl.revision = (((hackage.mtl)."2.2.2").revisions).default;
        ghc-bignum.revision = (((hackage.ghc-bignum)."1.3").revisions).default;
        ghc-prim.revision = (((hackage.ghc-prim)."0.8.0").revisions).default;
        base.revision = (((hackage.base)."4.16.4.0").revisions).default;
        rts.revision = (((hackage.rts)."1.0.2").revisions).default;
        transformers.revision = (((hackage.transformers)."0.5.6.2").revisions).default;
        };
      compiler = {
        version = "9.2.8";
        nix-name = "ghc928";
        packages = {
          "mtl" = "2.2.2";
          "ghc-prim" = "0.8.0";
          "base" = "4.16.4.0";
          "ghc-bignum" = "1.3";
          "rts" = "1.0.2";
          "transformers" = "0.5.6.2";
          };
        };
      };
  extras = hackage:
    { packages = { hello-world = ./.plan.nix/hello-world.nix; }; };
  modules = [
    ({ lib, ... }:
      { packages = { "hello-world" = { flags = {}; }; }; })
    ({ lib, ... }:
      {
        packages = {
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "hello-world".components.exes."hello-world".planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }
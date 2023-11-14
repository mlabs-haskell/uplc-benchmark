{
  pkgs = hackage:
    {
      packages = {
        ghc-bignum.revision = (((hackage.ghc-bignum)."1.3").revisions).default;
        ghc-prim.revision = (((hackage.ghc-prim)."0.9.0").revisions).default;
        base.revision = (((hackage.base)."4.17.1.0").revisions).default;
        rts.revision = (((hackage.rts)."1.0.2").revisions).default;
        };
      compiler = {
        version = "9.4.5";
        nix-name = "ghc945";
        packages = {
          "ghc-prim" = "0.9.0";
          "base" = "4.17.1.0";
          "ghc-bignum" = "1.3";
          "rts" = "1.0.2";
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
          "base".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }
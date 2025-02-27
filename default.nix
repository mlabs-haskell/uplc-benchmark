{ lib, ... }: {
  hercules-ci.github-pages.branch = "master";
  perSystem = { self', config, pkgs, ... }:
    let
      findCabalVersion = dep:
        (lib.findFirst
          (p: p.identifier.name == dep)
          (throw "No ${dep} in dependencies")
          self'.packages.${"${dep}-implementation-export"}.passthru.config.depends).identifier.version;

      versionsFile = pkgs.runCommand "versions.md" { } ''
        { echo - plutus-tx: ${findCabalVersion "plutus-tx"}
          echo - plutarch: ${findCabalVersion "plutarch"}
          echo - aiken: $(${config.libAiken.packages.aiken}/bin/aiken --version | cut -d ' ' -f 2)
          echo - opshin: $(${pkgs.opshin}/bin/opshin --version | cut -d ' ' -f 2)
        } > $out
      '';
    in
    {
      hercules-ci.github-pages.settings.contents = config.packages.website;

      mdBook = {
        website = {
          src = ./.;
          preBuild = ''
            mkdir -p static
            cp ${self'.packages.nft-marketplace-specification}/nft-marketplace.pdf static/nft-marketplace-spec.pdf
            cp ${self'.packages.dex-specification}/dex.pdf static/constant-product-dex-spec.pdf
            cp ${self'.packages.data-files}/* .
            cp ${versionsFile} versions.md

            for f in *.md; do
                sed --in-place "/<!-- $f -->/r $f" comparison.md
            done
          '';
        };
      };
    };
}

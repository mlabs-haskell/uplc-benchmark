{
  hercules-ci.github-pages.branch = "master";
  perSystem = { self', config, ... }: {
    hercules-ci.github-pages.settings.contents = config.packages.website;

    mdBook = {
      website = {
        src = ./.;
        preBuild = ''
          mkdir -p static
          cp ${self'.packages.nft-marketplace-specification}/nft-marketplace.pdf static/nft-marketplace-spec.pdf
          cp ${self'.packages.dex-specification}/dex.pdf static/constant-product-dex-spec.pdf
          cp ${self'.packages.data-files}/* .

          for f in *.md; do
              sed --in-place "/<!-- $f -->/r $f" comparison.md
          done
        '';
      };
    };
  };
}

{
  hercules-ci.github-pages.branch = "master";
  perSystem = { self', config, ... }: {
    hercules-ci.github-pages.settings.contents = config.packages.website;

    mdbook = {
      website = {
        src = ./.;
        preBuild = ''
          mkdir -p static
          cp ${self'.packages.nft-marketplace-specification}/nft-marketplace.pdf static/nft-marketplace-spec.pdf
          echo '- [NFT Marketplace](./static/nft-marketplace-spec.pdf)' >> specifications.md
        '';
      };
    };
  };
}

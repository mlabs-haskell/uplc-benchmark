# uplc-benchmark

See [website](https://mlabs-haskell.github.io/uplc-benchmark).

## Components

### Specification

Validator specifications are written in LaTeX and sources are available in this repository under `./specifications` directory.

- [dex](./specifications/dex)
- [nft-marketplace](./specifications/nft-marketplace)

Compiled PDF artifacts are available for download from the projects [website](https://mlabs-haskell.github.io/uplc-benchmark/specifications.html). These are automatically rebuilt on each push to `master` branch.

### Implementation

Aforementioned specification had been implemented in four Cardano languages that compile to UPLC. Plutarch implementation is considered the reference one but all implementations are tested in the same test framework.

- [Plutarch](./implementations/plutarch)
- [PlutusTx](./implementations/plutus-tx)
- [Opshin](./implementations/opshin)
- [Aiken](./implementations/aiken)

### Results (in progress)

## Building

Whole project uses [Nix](https://nixos.org/) as a build system with [Flakes](https://nixos.wiki/wiki/Flakes) enabled. Although various dev shells will provide with native tooling such as `cabal` or `python` they are used only for development environment and all builds happen inside Nix sandbox.

Each component comes with its own "Usage" section that describes both development and building commands. Throughout this repository it is assumed that user is in a dev shell provided for each of the components in `.envrc` files, loading these can be automated with [nix-direnv](https://github.com/nix-community/nix-direnv).

While not required, it is recommended to use [IOG Nix Binary Cache](https://github.com/input-output-hk/iogx/blob/8f26fda1b8dd97898b968c78598513e5648e84d9/doc/nix-setup-guide.md#setting-up-iog-binary-cache) to avoid building multiple GHC versions.

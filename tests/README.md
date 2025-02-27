# Unified test bench

This project contains language-agnostic unit tests.

## Usage

Dev shell will provide environment variables that point to validator implementations. For testing purposes they can be overwritten locally. Env variables must point to directories with `.bin` files.
- `UPLC_BENCHMARK_BIN_PLUTARCH`
- `UPLC_BENCHMARK_BIN_AIKEN`
- `UPLC_BENCHMARK_BIN_PLUTUS_TX`
- `UPLC_BENCHMARK_BIN_OPSHIN`

Test runs also export data files (can be obtained with `nix build .#data-files`) used to populate graphs and tables on the [website](https://mlabs-haskell.github.io/uplc-benchmark/comparison.html).

### Nix

```console
$ nix check
```

### Native (development only)

Make sure you are in the development shell from `.envrc` file.

```console
$ cabal test
```

# PlutusTx Implementation

Implementation of benchmarked validators in [PlutusTx](https://github.com/IntersectMBO/plutus/tree/master/plutus-tx).

## Usage

Output will have binary files with compiled UPLC code

### Nix

```
$ nix build .#plutus-tx-implementation
```

### Native (development only)

Make sure you are in the development shell from `.envrc` file.

```
$ cabal run
```

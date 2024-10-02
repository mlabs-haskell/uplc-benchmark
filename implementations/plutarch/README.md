# Plutarch Implementation

Reference implementation of benchmarked validators in [Plutarch](https://github.com/Plutonomicon/plutarch-plutus).

## Usage

### Nix

Output will have binary files with compiled UPLC code

```
$ nix build .#plutarch-implementation
```

### Native (development only)

Make sure you are in the development shell from `.envrc` file.

```
$ cabal run
```

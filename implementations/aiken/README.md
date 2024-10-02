# Aiken Implementation

Implementation of benchmarked validators in [Aiken](https://github.com/aiken-lang/aiken).

## Usage

### Nix

Aiken doesn't come with build sandbox support (will try to fetch files from the internet which is blocked by the sandbox) and its lock file does not include hashes so we have developed our own build system and lock file solution. If dependency change you may need to run `aiken2nix` to regenerate lock files.

```
$ aiken2nix # only if 'aiken.lock' changed
$ nix build .#aiken-implementation-compiled
```

### Native (development only)

Make sure you are in the development shell from `.envrc` file.

```
$ aiken build
```

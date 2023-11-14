# Plutarch Implementation

It is just a template to test compilation setup. Nothing to see here

# Updating dependencies

After changing dependencies in cabal file make sure to run an update script to update files in `.materialized` directory.
```
$ nix run .#'<name>:update'
```

#!/usr/bin/env bash

set -euxo pipefail

cd nix
rm no-gc-outs-*
rm no-gc-drvs-*
nix-instantiate ../default.nix -A noGcDeps --indirect --add-root no-gc-drvs
nix-store --indirect --add-root no-gc-outs --realise $(nix-instantiate ../default.nix -A noGcDeps)

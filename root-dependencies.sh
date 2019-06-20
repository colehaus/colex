#!/usr/bin/env bash

set -euxo pipefail

cd nix
nix-instantiate ../default.nix -A noGcDeps --indirect --add-root no-gc-drvs
nix-store --indirect --add-root no-gc-outs --realise $(nix-instantiate ../default.nix -A noGcDeps)

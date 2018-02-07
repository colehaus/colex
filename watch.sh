#! /usr/bin/env nix-shell
#! nix-shell -i bash default.nix
set -euxo pipefail
cd content
site watch &
cd js
NODE_PATH="$NODE_DEPENDENCIES"/lib/node_modules "$NODE_DEPENDENCIES"/bin/webpack &
trap 'kill $(jobs -p)' EXIT
sleep infinity

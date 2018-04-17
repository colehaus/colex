#! /usr/bin/env nix-shell
#! nix-shell -i bash shell.nix

set -euxo pipefail
cd hakyll
stack build --nix
cd ../content/js
rm -f dist/*
# Default `NODE_PATH` doesn't work with scoped packages
for path in ${NODE_PATH//:/ }; do
    if [[ "$path" = *"node-dependencies-ColEx"* ]]; then
        NODE_PATH="$path/@colehaus":$NODE_PATH
    fi
done
# Fixes race between hakyll and webpack
NO_WATCH=no_watch webpack
cd ..
stack exec site watch --nix &
cd js
webpack &
cd bibliometric
bower install
pulp --watch browserify --to ../dist/bibliometric.js &
trap 'kill $(jobs -p)' EXIT
sleep infinity

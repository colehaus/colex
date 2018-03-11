#! /usr/bin/env nix-shell
#! nix-shell -i bash shell.nix
set -euxo pipefail
cd hakyll
stack build
cd ../content/js
rm dist/*
# Fixes race between hakyll and webpack
NO_WATCH=no_watch webpack
cd ..
stack exec site clean
stack exec site watch &
cd js
webpack &
trap 'kill $(jobs -p)' EXIT
sleep infinity

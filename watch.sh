#! /usr/bin/env nix-shell
#! nix-shell -i bash shell.nix

set -euxo pipefail
cd hakyll
stack build --nix
cd ../content/js
rm -f dist/*
# Fixes race between hakyll and webpack
NO_WATCH=no_watch webpack
cd ..
stack exec site clean --nix
stack exec site watch --nix &
cd js
webpack &
trap 'kill $(jobs -p)' EXIT
sleep infinity

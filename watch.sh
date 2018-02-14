#! /usr/bin/env nix-shell
#! nix-shell -i bash shell.nix
set -euxo pipefail
cd hakyll
stack build
cd ../content
stack exec site watch &
cd js
webpack &
cd bibliometric
pulp --watch browserify --to ../dist/bibliometric.js &
trap 'kill $(jobs -p)' EXIT
sleep infinity

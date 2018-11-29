#! /usr/bin/env nix-shell
#! nix-shell -i bash shell.nix

set -euxo pipefail

root=$(pwd)

ijsinstall
jupyter notebook --no-browser &

cd "${root}/hakyll"
stack install

cd "${root}/content/js"
rm -f dist/*

# Default `NODE_PATH` doesn't work with scoped packages
# e.g. `webpack-cli` expects `flow-webpack-plugin` to be a direct child of one of the `NODE_PATH` entries
for path in ${NODE_PATH//:/ }; do
    if [[ "${path}" = *"node-dependencies-ColEx"* ]]; then
        NODE_PATH="${path}/@colehaus":$NODE_PATH
        # babel doesn't read NODE_PATH so this seems like the most convenient way to tell it about required deps
        sudo rm -rf node_modules
        mkdir node_modules
        ln -s "${path}/@babel" node_modules/@babel
    fi
done

# Fixes race between hakyll and webpack
NO_WATCH=no_watch webpack

cd "${root}/content"
"${root}/hakyll/.stack-bin/site" watch &

cd "${root}/content/js"
webpack &

# cd "${root}/content/js/bibliometric"
# bower install
# pulp --watch browserify --to ../dist/bibliometric.js &

# cd "${root}/content/js/value-of-information-calculator"
# npm install
# bower install
# pulp --watch browserify --to ../dist/value-of-information-calculator.js &

# cd "${root}/content/js/construct-vnm-utility-function"
# bower install
# npm install
# pulp --watch browserify --to ../dist/construct-vnm-utility-function.js &

cd "${root}/content/js/exemplars-curse"
bower install
npm install
pulp --watch browserify --to ../dist/exemplars-curse.js &

trap 'kill $(jobs -p)' EXIT
sleep infinity

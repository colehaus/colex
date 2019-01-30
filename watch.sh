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

# Fixes race between hakyll and webpack
NO_WATCH=no_watch webpack

cd "${root}/content"
"${root}/hakyll/.stack-bin/site" watch &

cd "${root}/content/js"
webpack &

purescript_project () {
    cd "${root}/content/js/${1}"
    npm install
    bower install
    pulp --watch browserify --to "../dist/${1}.js" &
}

declare -a purescript_projects=(
    # "bibliometric"
    # "value-of-information-calculator"
    # "construct-vnm-utility-function"
    # "exemplars-curse"
    "dominated-decisions"
    "priority-decisions"
)

for project in "${purescript_projects[@]}"
do
  purescript_project "$project"
done

trap 'kill $(jobs -p)' EXIT
sleep infinity

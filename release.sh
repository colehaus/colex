#!/usr/bin/env bash

set -euxo pipefail

git checkout master
nix-build -o colex-result
git checkout gh-pages
sudo cp -r colex-result/* .
git add -i
git commit -m "Bump"
git push

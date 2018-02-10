#!/usr/bin/env bash

set -euxo pipefail

git checkout master
revision=`git log -n 1 --pretty=format:"%H"`
comment=`git log -n 1 --pretty=format:"%B"`
access_token=`cat ./rollbar-access-token.txt`
environment=production
nix-build -o colex-result
git checkout gh-pages
sudo cp -r colex-result/* .
git add -i
git diff-index --quiet HEAD || git commit -m "Bump"
git push
curl https://api.rollbar.com/api/1/deploy/ \
     -F access_token=$access_token \
     -F environment=$environment \
     -F revision=$revision \
     -F comment="$comment"

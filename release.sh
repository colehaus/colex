#!/usr/bin/env bash

set -euxo pipefail

git checkout master
revision=`git log --max-count=1 --pretty=format:"%H"`
comment=`git log --max-count=1 --pretty=format:"%B"`
access_token=`cat ./rollbar-access-token.txt`
environment=production
nix-build
cd ../colehaus.github.io
sudo rm -r *
shopt -s extglob
sudo cp -r ../colex/result/* .
git add --interactive
git diff-index --quiet HEAD || git commit --no-verify --message "Bump"
git push
curl https://api.rollbar.com/api/1/deploy/ \
     -F access_token=$access_token \
     -F environment=$environment \
     -F revision=$revision \
     -F comment="$comment"

#!/usr/bin/env bash

set -eux

git config user.name "GitHub Actions"
git config user.email "actions@user.noreply.github.com"

BRANCH=feature/rebar3-depup-updates

if git branch -a | grep "$BRANCH" > /dev/null; then
    # already exists
    exit
fi

git fetch origin
git checkout -b "$BRANCH"
mkdir -p "$HOME/.config/rebar3"
echo "{plugins, [rebar3_depup]}." > "$HOME/.config/rebar3/rebar.config"
rebar3 up

if ! git diff --exit-code 1> /dev/null ; then
    # there's stuff to push
    git add rebar.config
    git commit -m "Update rebar.config versions (rebar3 depup)"
    git push origin "$BRANCH"

    gh pr create --fill \
        --title "Update rebar.config versions with rebar3_depup (automation)" \
        --body "This is an automated action to update the repositories rebar.config versions"
fi

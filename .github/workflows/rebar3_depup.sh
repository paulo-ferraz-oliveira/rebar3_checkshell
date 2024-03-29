#!/usr/bin/env bash

set -eux

git config user.name "GitHub Actions"
git config user.email "actions@user.noreply.github.com"

git fetch origin
BRANCHES=$(git branch -a)
BRANCH=feature/rebar3-depup-updates
if echo "${BRANCHES}" | grep "${BRANCH}" >/dev/null; then
    # exists
    exit
fi

git checkout -b "${BRANCH}"
mkdir -p "${HOME}/.config/rebar3"
echo "{plugins, [rebar3_depup]}." >"${HOME}/.config/rebar3/rebar.config"
rebar3 update-deps --replace
rebar3 upgrade --all
rebar3 fmt

if ! git diff --exit-code >/dev/null; then
    # there's stuff to push
    TITLE="[automation] Update \`rebar.config\` versions (via \`rebar3 depup\`)"

    git add rebar.config
    git add rebar.lock
    git commit -m "${TITLE}"
    git push origin "${BRANCH}"

    gh pr create --fill \
        --title "${TITLE}" \
        --body "This is an automated action to update the repository's \`rebar.config\` versions."
fi

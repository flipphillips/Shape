#!/usr/bin/env bash
#

version=`jq -j '.major_version,".",.minor_version,".",.revision_number' version.json` 
buildno=`jq '.build_number' version.json`
gitversion=`git rev-parse --short HEAD`
echo "${PWD##*/} Source Information"
echo "Current Version: v${version}-${buildno} (git rev ${gitversion})"
echo "Last commit: `git log -1 --format=%cd`"

# gitstatus
UPSTREAM=${1:-'@{u}'}
LOCAL=$(git rev-parse @)
REMOTE=$(git rev-parse "$UPSTREAM")
BASE=$(git merge-base @ "$UPSTREAM")

git remote -v update

if [ $LOCAL = $REMOTE ]; then
    echo "Up-to-date"
    exit 0
elif [ $LOCAL = $BASE ]; then
    echo "Need to pull"
    exit 1
elif [ $REMOTE = $BASE ]; then
    echo "Need to push"
    exit 2
else
    echo "Diverged"
    exit 3
fi

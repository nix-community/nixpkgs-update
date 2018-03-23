#!/usr/bin/env bash
#set -euxo pipefail

./check-attrpath-version.sh "libgit2_0_25" "0.25.3"

./check-attrpath-version.sh "owncloud90" "9.0.3"

if ./check-attrpath-version.sh "owncloud90" "9.1.3"
then
    echo "fail"
    exit 1
fi


echo "All tests passed"

#! /usr/bin/env bash
set -euxo pipefail

LOG_FILE=~/.nix-update/ups.log
mkdir -p $(dirname $LOG_FILE)
touch $LOG_FILE

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

ARGUMENTS=""

export DRY_RUN=true

IFS=$'\n'
for a in $ARGUMENTS
do
    unset IFS
    if eval "$SCRIPT_DIR/up.sh $a"
    then
        echo "$a SUCCESS" >> $LOG_FILE
    else
        echo "$a FAIL" >> $LOG_FILE
    fi
done

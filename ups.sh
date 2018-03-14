#! /usr/bin/env bash
set -euxo pipefail

LOG_FILE=~/.nix-update/ups.log
mkdir -p $(dirname $LOG_FILE)
touch $LOG_FILE

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

ARGUMENTS=""

export DRY_RUN=true
echo "

$(date -Iseconds) New run of ups.sh" >> $LOG_FILE

IFS=$'\n'
for a in $ARGUMENTS
do
    unset IFS
    echo -n "$(date -Iseconds) $a " >> $LOG_FILE
    if eval "$SCRIPT_DIR/up.sh $a"
    then
        echo "SUCCESS at $(date -Iseconds)" >> $LOG_FILE
        sleep 900
    else
        echo "FAIL at $(date -Iseconds)" >> $LOG_FILE
    fi
done

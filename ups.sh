#!/usr/bin/env bash
set -euxo pipefail

LOG_FILE=~/.nix-update/ups.log
mkdir -p "$(dirname $LOG_FILE)"
touch $LOG_FILE

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

GITHUB_TOKEN="$(cat "$SCRIPT_DIR"/github_token.txt)"
export GITHUB_TOKEN

ARGUMENTS=$(cat packages-to-update.txt)

# shellcheck source=setup-nixpkgs.sh
source "$SCRIPT_DIR/setup-nixpkgs.sh"

echo "

$(date -Iseconds) New run of ups.sh" >> $LOG_FILE

OK_TO_PR_AT=0
IFS=$'\n'
for a in $ARGUMENTS
do
    unset IFS
    echo "$(date -Iseconds) $a" >> $LOG_FILE

    if eval "$SCRIPT_DIR/up.sh $a $OK_TO_PR_AT 3>>$LOG_FILE"
    then
        RESULT=$?
    else
        RESULT=1
    fi

    case "$RESULT" in
        0)
            echo "$(date -Iseconds) SUCCESS" >> $LOG_FILE
            OK_TO_PR_AT=$(date +%s -d "+15 minutes")
        ;;
        1)
            echo "$(date -Iseconds) FAIL" >> $LOG_FILE
        ;;
        *) exit 1
        ;;
    esac
done

echo "$(date -Iseconds) ups.sh finished" >> $LOG_FILE

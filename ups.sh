#! /usr/bin/env bash
set -euxo pipefail

LOG_FILE=~/.nix-update/ups.log
mkdir -p "$(dirname $LOG_FILE)"
touch $LOG_FILE

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

ARGUMENTS=""

NIXPKGS=$HOME/.cache/nixpkgs
if ! [ -d "$NIXPKGS" ]
then
    hub clone nixpkgs "$NIXPKGS" # requires that user has forked nixpkgs
    cd "$NIXPKGS"
    git remote add upstream https://github.com/NixOS/nixpkgs
    git fetch upstream
    git fetch origin staging
    git fetch upstream staging
fi

cd "$NIXPKGS"

export DRY_RUN=true
echo "

$(date -Iseconds) New run of ups.sh" >> $LOG_FILE

IFS=$'\n'
for a in $ARGUMENTS
do
    unset IFS
    echo "$(date -Iseconds) $a" >> $LOG_FILE
    if eval "$SCRIPT_DIR/up.sh $a 3>>$LOG_FILE"
    then
        echo "$(date -Iseconds) SUCCESS" >> $LOG_FILE
        sleep 900
    else
        echo "$(date -Iseconds) FAIL" >> $LOG_FILE
    fi
done

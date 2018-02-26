#! /usr/bin/env bash
set -euxo pipefail

LOG_FILE=~/.nix-update/ups.log

mkdir -p $(dirname $LOG_FILE)
touch $LOG_FILE

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

ARGUMENTS=" clisp 2.50pre20171114 2.49.90
 cloog-ppl 0.15.11 0.16.1
 closure-compiler 20170910 20180204
 cloud-init 0.7.9 17.2
 cloudfoundry-cli 6.32.0 6.34.1
 cmark 0.27.1 0.28.3
 cminpack 1.3.4 1.3.6
 cmst 2017.09.19 2018.01.06
 cni 0.5.2 0.6.0
 cockroach 1.1.2 1.1.5
 codeblocks 16.01 17.12
 comfortaa 2.004 3.100"


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


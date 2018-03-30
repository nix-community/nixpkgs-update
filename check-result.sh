#! /usr/bin/env bash
set -euxo pipefail

RESULT_PATH=$1
EXPECTED_VERSION=$2
LOG_FILE=~/.nix-update/check-result-log.tmp

EDITOR="echo"
export EDITOR

OLD_HOME=~/
HOME=/homeless-shelter

pushd "$(mktemp -d)" >/dev/null

rm -f $LOG_FILE

function check_binary_help()
{
    if timeout -k 2 1 "$1" "$2" 2>/dev/null 1>/dev/null
    then
        echo "- ran \`$1 $2\` got 0 exit code" >> $LOG_FILE
    fi
}

function check_version_type()
{
    if timeout -k 2 1 "$1" "$2" 2>&1 | tr '\n' ' ' | grep -qE "[^\\.]+$EXPECTED_VERSION(\\.)*[[:space:]]*"
    then
        echo "- ran \`$1 $2\` and found version $EXPECTED_VERSION" >> $LOG_FILE
    fi
}

function check_binary() {

    check_binary_help "$1" "-h"
    check_binary_help "$1" "--help"
    check_binary_help "$1" "help"

    check_version_type "$1" "-V"
    check_version_type "$1" "-v"
    check_version_type "$1" "--version"
    check_version_type "$1" "version"
    check_version_type "$1" "-h"
    check_version_type "$1" "--help"
    check_version_type "$1" "help"

}

BINARIES=$(find "$RESULT_PATH"/bin -type f || true)

for b in $BINARIES
do
    check_binary "$b"
done

if [ -s $LOG_FILE ]
then
    true
else
    echo "- Warning: no binary found that responded to help or version flags. (This warning appears even if the package isn't expected to have binaries.)" >> $LOG_FILE
fi

if grep -r "$EXPECTED_VERSION" "$RESULT_PATH" >/dev/null
then
    echo "- found $EXPECTED_VERSION with grep in $RESULT_PATH" >> $LOG_FILE
fi

if find "$RESULT_PATH" -type f -printf '%f\n' | grep "$EXPECTED_VERSION" >/dev/null
then
    echo "- found $EXPECTED_VERSION in filename of file in $RESULT_PATH" >> $LOG_FILE
fi

HOME="$OLD_HOME"

GIST=$(tree "$RESULT_PATH" | gist || "")
if [ -n "$GIST" ]
then
   echo "- directory tree listing: $GIST" >> $LOG_FILE
fi

popd >/dev/null

cat $LOG_FILE || true

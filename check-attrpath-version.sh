#!/usr/bin/env bash
set -euxo pipefail

ATTR_PATH=$1
NEW_VERSION=$2

HAS_UNDERSCORES=$(echo "$ATTR_PATH" | grep "_")

if [ -n "$HAS_UNDERSCORES" ]
then
    ATTR_VERSION_PART=$(echo "$ATTR_PATH" | sed -n 's/^[^_]*_\([0-9_]*\)$/\1/p')

    # If we don't find version numbers in the attr path, exit success.
    if [ -z "$ATTR_VERSION_PART" ]
    then
        exit 0
    fi

    # Check assuming version part has underscore separators
    ATTR_VERSION_PERIODS=$(echo "$ATTR_VERSION_PART" | sed -n 's/_/\./gp')

    if [ -n "$ATTR_VERSION_PERIODS" ]
    then
        if [[ "$NEW_VERSION" == $ATTR_VERSION_PERIODS* ]]
        then
            exit 0
        fi
    fi
else
    ATTR_VERSION_PART=$(echo "$ATTR_PATH" | sed -n 's/^[^0-9]*\([0-9]*\)$/\1/p')

    # If we don't find version numbers in the attr path, exit success.
    if [ -z "$ATTR_VERSION_PART" ]
    then
        exit 0
    fi

    # Check assuming version part is the prefix of the version with dots
    # removed. For example, 91 => "9.1"
    NO_PERIOD_NEW_VERSION=$(echo "$NEW_VERSION" | sed -n 's/\.//gp')

    if [ -n "$NO_PERIOD_NEW_VERSION" ]
    then
        if [[ "$NO_PERIOD_NEW_VERSION" == $ATTR_VERSION_PART* ]]
        then
            exit 0
        fi
    fi
fi

exit 1

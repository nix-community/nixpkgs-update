#! /usr/bin/env bash
set -euxo pipefail

function error_exit {
    cleanup
    echo "$(date -Iseconds) $1" >&3
    exit 1
}

function error_exit {
    cleanup
    echo "$(date -Iseconds) $1" >&3
    exit 1
}

function push() {
    git push --set-upstream origin "$BRANCH_NAME" --force
}

function pull_request() {
    if [[ -v DRY_RUN ]]
    then
	return 0
    fi

    # Try to push it three times
    push || push || push

    GITHUB_TOKEN="$(cat "$SCRIPT_DIR"/github_token.txt)" hub pull-request -m "$1"
}

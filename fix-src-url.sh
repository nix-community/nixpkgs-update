#!/usr/bin/env bash
set -euxo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

GITHUB_TOKEN="$(cat "$SCRIPT_DIR"/github_token.txt)"
export GITHUB_TOKEN

# shellcheck source=setup-nixpkgs.sh
source "$SCRIPT_DIR/setup-nixpkgs.sh"

PACKAGE_NAME=$1
OLD_VERSION=$2
NEW_VERSION=$3
DERIVATION_FILE=$4
ATTR_PATH=$5
OLD_SRC_URL=$6


DOWNLOADS=$(curl "https://repology.org/api/v1/metapackage/$PACKAGE_NAME" | jq '.[].downloads | select(values) | .[] ' | grep "$NEW_VERSION" | sed 's|"||g')

# if [ -z "$DOWNLOADS" ]
# then
#     exit 1
# fi

OLD_DERIVATION_NAME=$(nix eval -f ~/p/nixpkgs --raw "pkgs.$ATTR_PATH.name")
NEW_DERIVATION_NAME=$(sed "s|$OLD_VERSION|$NEW_VERSION|" <<< "$OLD_DERIVATION_NAME")
NAME=$(nix eval --raw "(let pkgs = import ./. {}; in (builtins.parseDrvName pkgs.$ATTR_PATH.name).name)")

if grep -q "name = \"$NEW_DERIVATION_NAME\"" "$DERIVATION_FILE"
then
    # Separate name and version
    sed -i "s|$NEW_DERIVATION_NAME|$NAME-\${version}|" "$DERIVATION_FILE"
    grep -q "name = \"$NAME-\${version}\"" "$DERIVATION_FILE"
    # shellcheck disable=SC2016
    sed -i 's|^\([ ]*\)\(name = "'"$NAME"'-\${version}";\)|\1\2\n\1version = "'"$NEW_VERSION"'";|' "$DERIVATION_FILE"
    grep -q "version = \"$NEW_VERSION\";" "$DERIVATION_FILE"
fi

for d in $DOWNLOADS
do
    OLD_URL="$OLD_SRC_URL"
    OLD_URL=$(sed "s|$OLD_DERIVATION_NAME|\${name}|" <<< "$OLD_URL")
    OLD_URL=$(sed "s|$OLD_VERSION|\${version}|" <<< "$OLD_URL")

    NEW_URL=$(sed "s|$NEW_DERIVATION_NAME|\${name}|" <<< "$d" | sed "s|$NEW_VERSION|\${version}|")
    sed -i "s|$OLD_URL|$NEW_URL|" "$DERIVATION_FILE"
    grep -q 'url = "'"$NEW_URL"'";' "$DERIVATION_FILE" || continue

    nix-prefetch-url -A "$ATTR_PATH.src" && exit 0
done

exit 1

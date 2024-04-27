# Nixpkgs Maintainer FAQ {#nixpkgs-maintainer-faq}

## @r-ryantm opened a PR for my package, what do I do?

Thanks for being a maintainer. Hopefully, @r-ryantm will be able to save you some time!

1. Review the PR diff, making sure this update makes sense
   - sometimes updates go backward or accidentally use a dev version
2. Review upstream changelogs and commits
3. Follow the "Instructions to test this update" section of the PR to get the built program on your computer quickly
4. Make a GitHub Review approving or requesting changes. Include screenshots or other notes as appropriate.

## Why is @r-ryantm not updating my package? {#no-update}

There are lots of reasons a package might not be updated. You can usually figure out which one is the issue by looking at the [logs](https://r.ryantm.com/log/) or by asking @ryantm on Matrix or GitHub.

### No new version information

r-ryantm gets its new version information from three sources:

* Repology - information from Repology is delayed because it only updates when there is an unstable channel release
* GitHub releases
* PyPi releases

If none of these sources says the package is out of date, it will not attempt to update it.

### Disabling package updates

Updates can be disabled by adding a comment to the package:
```
# nixpkgs-update: no auto update
```
[Example in nixpkgs](https://github.com/NixOS/nixpkgs/blob/f2294037ad2b1345c5d9c2df0e81bdb00eab21f3/pkgs/applications/version-management/gitlab/gitlab-pages/default.nix#L7)

### Skiplist

We maintain a [Skiplist](https://github.com/ryantm/nixpkgs-update/blob/main/src/Skiplist.hs) of different things not to update. It is possible your package is triggering one of the skip criteria.

Python updates are skipped if they cause more than 25 rebuilds.

### Existing Open or Draft PR

If there is an existing PR with the exact title of `$attrPath: $oldVersion -> $newVersion`, it will not update the package.

### Version not newer

If Nix's `builtins.compareVersions` does not think the "new" version is newer, it will not update.

### Incompatibile with "Path Pin"

Some attrpaths have versions appended to the end of them, like `ruby_3_0`, the new version has to be compatible with this "Path pin". Here are some examples:

```Haskell
-- >>> versionCompatibleWithPathPin "libgit2_0_25" "0.25.3"
-- True
--
-- >>> versionCompatibleWithPathPin "owncloud90" "9.0.3"
-- True
--
-- >>> versionCompatibleWithPathPin "owncloud-client" "2.4.1"
-- True
--
-- >>> versionCompatibleWithPathPin "owncloud90" "9.1.3"
-- False
--
-- >>> versionCompatibleWithPathPin "nodejs-slim-10_x" "11.2.0"
-- False
--
-- >>> versionCompatibleWithPathPin "nodejs-slim-10_x" "10.12.0"
-- True
```

### Can't find derivation file

If `nix edit $attrpath` does not open the correct file that contains the version string and fetcher hash, the update will fail.

This might not work, for example, if a package doesn't have a `meta` attr (at all, or if the package uses a builder function that is discarding the `meta` attr).

### Update already merged

If the update is already on `master`, `staging`, or `staging-next`, the update will fail.

### Can't find hash or source url

If the derivation file has no hash or source URL, it will fail.

Since `nixpkgs-update` is trying to read these from `<pkg>.src`, this can also happen if the package's source is something unexpected such as another package. You can set the fallback `originalSrc` attr so that `nixpkgs-update` can find the correct source in cases like this.

### No updateScript and no version

If the derivation file has no version and no updateScript, it will fail.

### No changes

If the derivation "Rewriters" fail to change the derivation, it will fail.

If there is no updateScript, and the source url or the hash did not change, it will fail.

### No rebuilds

If the rewrites didn't cause any derivations to change, it will fail.

### Didn't build

If after the rewrites, it doesn't build, it will fail.

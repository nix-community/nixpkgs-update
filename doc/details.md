# Details {#details}

Some of these features only apply to the update-list sub-command or to
features only available to the @r-ryantm bot.

## Checks

A number of checks are performed to help nixpkgs maintainers gauge the
likelihood that an update was successful. All the binaries are run with
various flags to see if they have a zero exit code and output the new
version number. The outpath directory tree is searched for files
containing the new version number. A directory tree and disk usage
listing is provided.


## Security report

Information from the National Vulnerability Database maintained by
NIST is compared against the current and updated package version. The
nixpkgs package name is matched with the Common Platform Enumeration
vendor, product, edition, software edition, and target software fields
to find candidate Common Vulnerabilities and Exposures (CVEs). The
CVEs are filtered by the matching the current and updated versions
with the CVE version ranges.

The general philosophy of the CVE search is to avoid false negatives,
which means we expect to generate many false positives. The false
positives can be carefully removed by manually created rules
implemented in the filter function in the NVDRules module.

If there are no CVE matches, the report is not shown. The report has
three parts: CVEs resolved by this update, CVEs introduced by this
update, and CVEs present in both version.

If you would like to report a problem with the security report, please
use the [nixpkgs-update GitHub
issues](https://github.com/nix-community/nixpkgs-update/issues).

The initial development of the security report was made possible by a
partnership with [Serokell](https://serokell.io/) and the [NLNet
Foundation](https://nlnet.nl/) through their [Next Generation Internet
Zero Discovery initiative](https://nlnet.nl/discovery/) (NGI0
Discovery). NGI0 Discovery is made possible with financial support
from the [European Commission](https://ec.europa.eu/).


## Rebuild report

The PRs made by nixpkgs-update say what packages need to be rebuilt if
the pull request is merged. This uses the same mechanism
[OfBorg](https://github.com/NixOS/ofborg) uses to put rebuild labels
on PRs. Not limited by labels, it can report the exact number of
rebuilds and list some of the attrpaths that would need to be rebuilt.


## PRs against staging

If a PR merge would cause more than 500 packages to be rebuilt, the PR
is made against staging.


## Logs

[Logs from r-ryantm's runs](https://nixpkgs-update-logs.nix-community.org/) are
available online. There are a lot of packages `nixpkgs-update`
currently has no hope of updating. Please dredge the logs to find out
why your pet package is not receiving updates.


## Cache

By serving the build outputs from
[https://nixpkgs-update-cache.nix-community.org/](https://nixpkgs-update-cache.nix-community.org/), nixpkgs-update allows you to
test a package with one command.

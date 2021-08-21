# nu

## Schema

## Table `package`

- id : int
- attrPath : string
- versionNixpkgsMaster : string
- versionNixpkgsStaging : string
- versionNixpkgsStagingNext : string
- versionRepology : string
- versionGitHub : string
- versionGitLab : string
- versionPypi : string
- projectRepology : string
- nixpkgsNameReplogy : string
- ownerGitHub : string
- repoGitHub : string
- ownerGitLab : string
- repoGitLab : string
- lastCheckedRepology : timestamp
- lastCheckedGitHub : timestamp
- lastCheckedGitLab : timestamp
- lastCheckedPyPi : timestamp
- lastCheckedPendingPR : timestamp
- lastUpdateAttempt : timestamp
- pendingPR : int
- pendingPROwner : string
- pendingPRBranchName : string
- lastUpdateLog : string

## Table `maintainer-package`

- id : int
- packageId : int
- maintainerId : int

## Table `maintainer`

- id : int
- gitHubName : string

# Change Log

The Skidmore Shape Package

![](icon.png)

<!--
## Guiding Principles

- Changelogs are for humans, not machines. 
- There should be an entry for every single version.
- The same types of changes should be grouped.
- Versions and sections should be linkable.
- The latest version comes first.
- The release date of each versions is displayed.
- Mention whether you follow Semantic Versioning.

## Types of changes

- `Added` for new features.
- `Changed` for changes in existing functionality.
- `Deprecated` for soon-to-be removed features.
- `Removed` for now removed features.
- `Fixed` for any bug fixes.
- `Security` in case of vulnerabilities. 
-->

## [Unreleased Changes]

### xxx

## [2.1.0] - 2018-02-27
### Added

- Metrics, Patches, Region, DepthMap
- Attic for 2D stuff
- Documentation in Readme
- LFS support
- .vscode directory
- Development directory with remnants + Fillip stuff

### Changed

- version in paclet and json
- init.wl - now dows loads witn `Needs` so it doesn't break parallel stuff.
- added `f[{k1,k2}]` wrappers for `f[k1,k2]` scenarios. I *thought* there was a way to do this with options.

### Removed

- Eclipse project file
- gitignore for `doc.xml`
- Documentation

### Fixed

- NormalizedContrast checks the parameters for 0-divide

## [2.0.0] - 2017-08-15
### Added
- this `CHANGELOG.md`
- version system

### Changed

- migrated from Workbench


## [1.0.0] - 2017-01-01

### Added

- everything from 1984 on.

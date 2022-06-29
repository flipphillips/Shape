# Change Log

The Skidmore now RIT Shape Package

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

## [WIP] - nee [3.0.0]

- 3.0 breaks `DifferentialGeometry` out into its own Paclet Manager / Repository system.
## [2.4.3]
### Changed
- Trying to fix some namespace errors in the `DepthMap` universe
- Debugging some build problems

## [2.4.1] - 15 March 2018

### Changed
- Refactored out of packages and into Paclets ala Arnoud's model
- version build bumper
- icon change
- file names for consistency
- rearrange of Development directory
- Renamed DepthMap stuff for consistency

### Added
- Volume tools
- Geometry tools
- 2D stuff to development
- Automatic versioning / paclet generation in build
- Globals w/ version strings and things


## [2.2.0] - skipped

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
- init.wl - now dows loads with `Needs` so it doesn't break parallel stuff.
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

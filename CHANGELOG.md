# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]


## [1.0.0] - 2019-11-26

### Added

- Clarified documentation for `deep_put/3` regarding possible exceptions.

### Changed

- `deep_remove` now removes the last existing key in the path, ignoring the rest
  to keep it in line with the behavior of `maps:remove/1` which silently returns
  the map if the key doesn't exist.

## [0.3.0] - 2018-09-13

Initial release

[unreleased]: https://github.com/eproxus/mapz/compare/v1.0.0...HEAD
[1.0.0]: https://github.com/eproxus/mapz/compare/v0.3.0...v1.0.0
[0.3.0]: https://github.com/eproxus/mapz/releases/tag/v0.3.0

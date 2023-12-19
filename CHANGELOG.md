# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [2.4.0] - 2023-11-13

### Added

- New [`deep_intersect/2`][deep_intersect-2] function
- New [`deep_intersect_with/3`][deep_intersect_with-3] function

[deep_intersect-2]: https://hexdocs.pm/mapz/mapz.html#deep_intersect-2
[deep_intersect_with-3]: https://hexdocs.pm/mapz/mapz.html#deep_intersect_with-3

### Fixed

- [`deep_merge_with/3`][deep_merge_with-3] now exits with `badarg` in case the
  fun is not valid.

## [2.3.0] - 2022-06-08

### Added

- New [`inverse/2`][inverse-2] function
- New [`deep_search/2`][deep_search-2] function

[inverse-2]: https://hexdocs.pm/mapz/mapz.html#inverse-2
[deep_search-2]: https://hexdocs.pm/mapz/mapz.html#deep_search-2

## [2.2.0] - 2021-06-17

### Added

- New [`deep_iterator/1`][deep_iterator-1] function
- New [`deep_next/1`][deep_next-1] function
- New [`deep_merge_with/2`][deep_merge_with-2] function
- New [`deep_merge_with/3`][deep_merge_with-3] function

[deep_iterator-1]: https://hexdocs.pm/mapz/mapz.html#deep_iterator-1
[deep_next-1]: https://hexdocs.pm/mapz/mapz.html#deep_next-1
[deep_merge_with-2]: https://hexdocs.pm/mapz/mapz.html#deep_merge_with-2
[deep_merge_with-3]: https://hexdocs.pm/mapz/mapz.html#deep_merge_with-3

## [2.1.1] - 2021-06-14

### Fixed

- Updating a path that had a map as value with `deep_update_with` returned
  `error` instead of the map value.

## [2.1.0] - 2020-07-02

### Added

- New [`deep_update_with/4`][deep_update_with-4] function

[deep_update_with-4]: https://hexdocs.pm/mapz/mapz.html#deep_update_with-4

## [2.0.0] - 2019-11-27

### Added

- New [`deep_update/3`][deep_update-3] function
- New [`deep_update_with/3`][deep_update_with-3] function

[deep_update-3]: https://hexdocs.pm/mapz/mapz.html#deep_update-3
[deep_update_with-3]: https://hexdocs.pm/mapz/mapz.html#deep_update_with-3

### Changed

- The `{badvalue, P}` exception from `deep_put/3` now returns a path to the bad
  value instead of the value itself to make it coherent with the new
  `deep_update/3` implementation.
- The `{badkey, K}` exception from `deep_get/2` has been changed to
  `{badvalue, P}` to make it coherent with `deep_put/3` and others.

## [1.0.0] - 2019-11-26

### Added

- Clarified documentation for `deep_put/3` regarding possible exceptions.

### Changed

- `deep_remove` now removes the last existing key in the path, ignoring the rest
  to keep it in line with the behavior of `maps:remove/1` which silently returns
  the map if the key doesn't exist.

## [0.3.0] - 2018-09-13

Initial release

[unreleased]: https://github.com/eproxus/mapz/compare/v2.4.0...HEAD
[2.4.0]: https://github.com/eproxus/mapz/compare/v2.3.0...v2.4.0
[2.3.0]: https://github.com/eproxus/mapz/compare/v2.2.1...v2.3.0
[2.2.0]: https://github.com/eproxus/mapz/compare/v2.1.1...v2.2.0
[2.1.1]: https://github.com/eproxus/mapz/compare/v2.1.0...v2.1.1
[2.1.0]: https://github.com/eproxus/mapz/compare/v2.0.0...v2.1.0
[2.0.0]: https://github.com/eproxus/mapz/compare/v1.0.0...v2.0.0
[1.0.0]: https://github.com/eproxus/mapz/compare/v0.3.0...v1.0.0
[0.3.0]: https://github.com/eproxus/mapz/releases/tag/v0.3.0

# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- `ident`, `block`, `path`, `lifetime` and `literal` type in `forr!` 
- `idents(prefix, count)` function to `forr!`
- `tuples($name:type, from..to)` function to `forr!`

### Changed
- **BREAKING** fully removed support for top level optional variables (they were supposed to be disallowed before but worked due to a bug)

## [0.2.3] - 2024-03-15
### Added
- `casing` utility to `forr!`

## [0.2.2] - 2023-10-28
### Added
- support for `#` instead of `$` inside of macros, when they are initialized with `#:`/`#*`

## [0.2.1] - 2023-10-01
### Added
- `equals_any` to `iff!`.
## [0.2.0] - 2023-09-09
### Added
- `iff!`.

## [0.1.1] - 2023-05-16
### Fixed
- Updated `proc-macro2` dependency to fix a parsing error.

## [v0.1.0] 
**Initial Release**

[unreleased]: https://github.com/ModProg/forr/compare/v0.2.3...HEAD
[0.2.3]: https://github.com/ModProg/forr/compare/v0.2.2...v0.2.3
[0.2.2]: https://github.com/ModProg/forr/compare/v0.2.1...v0.2.2
[0.2.1]: https://github.com/ModProg/forr/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/ModProg/forr/compare/v0.1.1...v0.2.0
[0.1.1]: https://github.com/ModProg/forr/compare/v0.1.0...v0.1.1
[v0.1.0]: https://github.com/ModProg/forr/tree/v0.1.0

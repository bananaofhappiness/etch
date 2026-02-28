# Changelog

All notable changes to this project will be documented in this file.

---
## [1.3.2] - 2026-03-01

### Fixes
- Fixed mouse event coordinates starting at (1,1) — now they correctly start at (0,0).
- Fixed styles not being applied properly.

### Added
- Added style examples to `dev/examples/styles.gleam`.

### Changed
- Updated style documentation.
- DRY: Unified `handle_events` function in examples/hello_world so both JavaScript and Erlang targets use the same implementation.

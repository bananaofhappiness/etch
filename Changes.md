# Changelog

All notable changes to this project will be documented in this file.

---
## [1.3.2] - 2026-02-28

### Fixes
- Fixed crash after receiving SIGWINCH (window resize signal).

### Changed
- Moved `examples/` directory to `dev/`.
- Updated documentation to match current API - removed or replaced all references to `command.EnterRaw` with `terminal.enter_raw()`.

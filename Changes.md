# Changelog

All notable changes to this project will be documented in this file.

---
## [1.3.0] - 2026-02-18

### Fixes
- Fixed parsing of special key codes (Enter, Backspace, Tab, Esc) in `parse_events`

### Changed
- **BREAKING**: Removed `EnterRaw` command from `etch/command`. Use `terminal.enter_raw()` directly instead
- **BREAKING**:  `terminal.window_size()` now returns `Result`. `terminal.enter_raw()` and `terminal.exit_raw()` return `Result` on JavaScript target.
- Added `TerminalError` type with `FailedToEnterRawMode`, `FailedToExitRawMode`, and `CouldNotGetWindowSize` variants
- CI now tests JavaScript target with Node, Deno, and Bun runtimes

### Known Issues

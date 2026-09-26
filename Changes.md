# Changelog

All notable changes to this project will be documented in this file.

---

## [1.4.1] - 2026-09-27

### Changed
- Updated the `etch/events` usage example in the documentation.

---

## [1.4.0] - 2026-05-28

### Breaking Changes

- Split into monorepo with target-specific packages: Erlang and JavaScript FFI code moved into separate `etch_erlang` and `etch_javascript` packages. The core `etch` package is now target-agnostic. See migration guide below.

### Changed

- Examples split into `examples_erlang` and `examples_javascript`.
- Bumped `gleam_std` dependency to v1.0.2.
- Fixed entering and exiting raw mode during program execution — now works flawlessly.

### Migration Guide (1.3.x → 1.4.0)

The `etch` package no longer contains target-specific FFI code. You now need to add the appropriate target package alongside `etch`.

**For Erlang targets:**

Add `etch_erlang` to your dependencies:

```toml
[dependencies]
etch = ">= 1.4.0 and < 2.0.0"
etch_erlang = ">= 1.0.0 and < 2.0.0"
```

**For JavaScript targets:**

Add `etch_javascript` to your dependencies:

```toml
[dependencies]
etch = ">= 1.4.0 and < 2.0.0"
etch_javascript = ">= 1.0.0 and < 2.0.0"
```

Application code imports need to be updated to reflect the new module structure:

- `enter_raw`, `exit_raw`, `is_raw_mode`, `window_size` → `etch/{target}/tty`
- `init_event_server`, `poll`, `read`, `get_cursor_position`, `get_keyboard_enhancement_flags` → `etch/{target}/input`

Where `{target}` is `erlang` or `javascript`.

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

---

## [1.3.1] - 2026-02-28

### Fixes
- Fixed crash after receiving SIGWINCH (window resize signal).

### Changed
- Moved `examples/` directory to `dev/`.
- Updated documentation to match current API - removed or replaced all references to `command.EnterRaw` with `terminal.enter_raw()`.

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

---

## [1.2.0] - 2025-12-18

### Fixes
- Fixed events order in standard (non-raw) mode. Previously typing "hi" will return event of "i", and then of "h". Now the order is correct.

### Added
- Added JavaScript target support.
- Added `exit_raw` and `is_raw_mode` functions.

### Known Issues

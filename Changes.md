# Changelog

All notable changes to this project will be documented in this file.

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

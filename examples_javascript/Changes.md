# Changelog

All notable changes to this project will be documented in this file.

---
## [1.1.0] - 2026-06-29

### Changed

- The event server is now started **lazily** on the first `poll`/`read` call so you no longer need to call `init_event_server()` manually before handling input.

### Removed

- `etch/javascript/input.{init_event_server}` — no longer public. The event server is started automatically the first time input is requested, so the manual setup step is gone.

### Migration

If you previously wrote:

```gleam
input.init_event_server()
```

simply delete that line — `poll` and `read` will start the event server for you
on first use.

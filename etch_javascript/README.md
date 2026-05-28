# etch_javascript

[![Package Version](https://img.shields.io/hexpm/v/etch_javascript)](https://hex.pm/packages/etch_javascript)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/etch_javascript/)

JavaScript target package for [etch](https://github.com/bananaofhappiness/etch) — a Gleam TUI backend library.

This package provides the JavaScript FFI implementations for terminal raw mode and input handling. It must be used alongside the core `etch` package.

## Installation

```sh
gleam add etch_javascript
```

## Usage

The package exposes two modules:

- **`etch/javascript/tty`** — Raw mode control and terminal information:
  - `enter_raw`, `exit_raw`, `is_raw_mode`
  - `window_size`

- **`etch/javascript/input`** — Event-driven input handling:
  - `init_event_server`, `poll`, `read`
  - `get_cursor_position`, `get_keyboard_enhancement_flags`

```gleam
import etch/javascript/tty
import etch/javascript/input

pub fn main() {
  let assert Ok(_) = tty.enter_raw()
  input.init_event_server()
  // ... handle events ...
}
```

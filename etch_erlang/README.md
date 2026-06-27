# etch_erlang

[![Package Version](https://img.shields.io/hexpm/v/etch_erlang)](https://hex.pm/packages/etch_erlang)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/etch_erlang/)

Erlang target package for [etch](https://github.com/bananaofhappiness/etch) — a Gleam TUI backend library.

This package provides the Erlang FFI implementations for terminal raw mode and input handling. It must be used alongside the core `etch` package.

## Installation

```sh
gleam add etch_erlang
```

## Usage

The package exposes two modules:

- **`etch/erlang/tty`** — Raw mode control and terminal information:
  - `enter_raw`, `exit_raw`, `is_raw_mode`
  - `window_size`

- **`etch/erlang/input`** — Event-driven input handling:
  - `poll`, `read`
  - `get_cursor_position`, `get_keyboard_enhancement_flags`

```gleam
import etch/erlang/tty

pub fn main() {
  let assert Ok(_) = tty.enter_raw()
  // ... handle events ...
}
```

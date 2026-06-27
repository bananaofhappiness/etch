import etch/erlang/tty.{is_raw_mode}
import etch/event.{
  type Event, type EventError, type KeyboardEnhancementFlag, FailedToParseEvent,
  parse_cursor_position, parse_events, parse_keyboard_enhancement_flags,
}
import etch/internal/consts.{csi}
import gleam/erlang/process.{type Pid}
import gleam/io
import gleam/option.{type Option}
import gleam/string

@external(erlang, "input_ffi", "start_link")
fn start_link() -> Nil

@external(erlang, "tty_state", "init")
fn init_tty_state() -> Nil

/// Checks if there is an [`Event`](https://hexdocs.pm/etch/etch/event.html#Event) available.
/// Returns None if no events were received within the timeout.
/// See also [`read`](input.html#read).
@external(erlang, "input_ffi", "poll")
pub fn poll(timeout: Int) -> Option(Result(Event, EventError))

/// Checks if there is an [`Event`](https://hexdocs.pm/etch/etch/event.html#Event) available.
/// Waits forever for an available event.
/// See also [`poll`](input.html#poll).
@external(erlang, "input_ffi", "read")
pub fn read() -> Option(Result(Event, EventError))

@external(erlang, "io", "get_chars")
fn get_chars(prompt: String, n: Int) -> String

@external(erlang, "io", "get_line")
fn erlang_read(prompt: String) -> String

@external(erlang, "input_ffi", "push")
fn push(event: Result(Event, EventError)) -> Nil

@internal
pub fn input_loop(is_raw_mode: Bool) {
  let str = case is_raw_mode {
    True -> get_chars("", 128)
    False -> erlang_read("")
  }
  let str = string.to_graphemes(str)
  let events = parse_events(str, "", [], False)
  push_events(events)
  input_loop(is_raw_mode)
}

fn push_events(events: List(Result(Event, EventError))) {
  case events {
    [] -> Nil
    [e, ..rest] -> {
      push(e)
      push_events(rest)
    }
  }
}

/// Get keyboard enhancement flags. See <https://sw.kovidgoyal.net/kitty/keyboard-protocol/#progressive-enhancement>
/// This function shouldn't be called in a tight loop. It's fine to call it when
/// responding to specific user input (e.g., after a key press), but avoid calling
/// it on every loop iteration.
pub fn get_keyboard_enhancement_flags() -> Result(
  List(KeyboardEnhancementFlag),
  EventError,
) {
  io.print(csi <> "?u")
  let flags = get_chars("", 32)
  case flags {
    "\u{001b}[?" <> s -> {
      case string.last(s) {
        Ok("u") -> Ok(parse_keyboard_enhancement_flags(s))
        _ -> Error(FailedToParseEvent("Could not get enhancment flags"))
      }
    }
    _ -> Error(FailedToParseEvent("Could not get cursor position"))
  }
}

/// Returns cursor position.
/// This function shouldn't be called in a tight loop. It's fine to call it when
/// responding to specific user input (e.g., after a key press), but avoid calling
/// it on every loop iteration.
pub fn get_cursor_position() -> Result(#(Int, Int), EventError) {
  io.print(csi <> "6n")
  let pos = get_chars("", 32)
  case pos {
    "\u{001b}[" <> s -> {
      case string.last(s) {
        Ok("R") -> parse_cursor_position(s)
        _ -> Error(FailedToParseEvent("Could not get cursor position"))
      }
    }
    _ -> Error(FailedToParseEvent("Could not get cursor position"))
  }
}

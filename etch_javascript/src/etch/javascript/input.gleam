import etch/event.{
  type Event, type EventError, type KeyboardEnhancementFlag, FailedToParseEvent,
  parse_events, parse_keyboard_enhancement_flags,
}
import gleam/javascript/array.{type Array}
import gleam/javascript/promise.{type Promise}
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string

@external(javascript, "./input_ffi.mjs", "get_chars")
fn get_chars() -> Promise(Array(Int))

@external(javascript, "./input_ffi.mjs", "push")
fn push(event: Result(Event, EventError)) -> Nil

@external(javascript, "./input_ffi.mjs", "poll")
fn poll_ffi(timeout: Int) -> Promise(Option(Result(Event, EventError)))

@external(javascript, "./input_ffi.mjs", "read")
fn read_ffi() -> Promise(Option(Result(Event, EventError)))

@external(javascript, "./input_ffi.mjs", "ensure_running")
fn ensure_running(fun: fn() -> Nil) -> Nil

/// Checks if there is an [`Event`](https://hexdocs.pm/etch/etch/event.html#Event) available.
/// Returns None if no events were received within the timeout.
/// See also [`read`](input.html#read).
pub fn poll(timeout: Int) -> Promise(Option(Result(Event, EventError))) {
  ensure_running(init_event_server)
  poll_ffi(timeout)
}

/// Checks if there is an [`Event`](https://hexdocs.pm/etch/etch/event.html#Event) available.
/// Waits forever for an available event.
/// See also [`poll`](input.html#poll).
pub fn read() -> Promise(Option(Result(Event, EventError))) {
  ensure_running(init_event_server)
  read_ffi()
}

@external(javascript, "./input_ffi.mjs", "handle_sigwinch")
fn handle_sigwinch() -> Nil

/// Initializes the event server responsible for listening for events.
fn init_event_server() {
  handle_sigwinch()
  input_loop()
  Nil
}

fn input_loop() {
  use bytes <- promise.await(get_chars())
  let bytes =
    array.to_list(bytes)
    |> list.map(fn(n) {
      let code =
        string.utf_codepoint(n)
        |> result.lazy_unwrap(fn() {
          let assert Ok(fallback) = string.utf_codepoint(65)
          fallback
        })
      string.from_utf_codepoints([code])
    })
  let events = parse_events(bytes, "", [], False)
  push_events(events)

  input_loop()
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

@external(javascript, "./input_ffi.mjs", "get_keyboard_enhancement_flags_code")
fn get_keyboard_enhancement_flags_code() -> Promise(Result(String, EventError))

/// Get keyboard enhancement flags. See <https://sw.kovidgoyal.net/kitty/keyboard-protocol/#progressive-enhancement>
/// This function shouldn't be called in a tight loop. It's fine to call it when
/// responding to specific user input (e.g., after a key press), but avoid calling
/// it on every loop iteration.
pub fn get_keyboard_enhancement_flags() -> Promise(
  Result(List(KeyboardEnhancementFlag), EventError),
) {
  use flags <- promise.await(get_keyboard_enhancement_flags_code())
  let res = case flags {
    Ok(code) -> Ok(parse_keyboard_enhancement_flags(code))
    _ -> Error(FailedToParseEvent("Could not get enhancment flags"))
  }
  promise.resolve(res)
}

/// Returns cursor position.
/// This function shouldn't be called in a tight loop. It's fine to call it when
/// responding to specific user input (e.g., after a key press), but avoid calling
/// it on every loop iteration.
@external(javascript, "./input_ffi.mjs", "get_cursor_position")
pub fn get_cursor_position() -> Promise(Result(#(Int, Int), EventError))

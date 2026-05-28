import etch/event.{type Event, type EventError, parse_events}
import gleam/erlang/process.{type Pid}
import gleam/string

pub type TerminalError {
  CouldNotGetWindowSize
  FailedToEnterRawMode
  FailedToExitRawMode
}

@external(erlang, "terminal_ffi", "enter_raw")
fn enter_raw_ffi() -> Result(Nil, TerminalError)

@external(erlang, "terminal_ffi", "exit_raw")
fn exit_raw_ffi() -> Result(Nil, TerminalError)

@external(erlang, "tty_state", "init")
@internal
pub fn tty_state_init() -> Nil

@external(erlang, "tty_state", "set_raw")
fn set_raw(is_raw: Bool) -> Nil

@external(erlang, "tty_state", "is_raw_mode")
pub fn is_raw_mode() -> Bool

/// Returns current window size.
@external(erlang, "terminal_ffi", "window_size")
pub fn window_size() -> Result(#(Int, Int), TerminalError)

/// Enters raw mode.
///
/// Raw mode is a mode where the terminal does not process input, but instead
/// passes it directly to the application. This means that:
/// - Input is not echoed to the screen
/// - Input is not line-buffered (characters are available immediately)
/// - Some special characters are not processed by the terminal
///
/// This is necessary for terminal UI applications that need to handle
/// keyboard input and mouse events directly.
pub fn enter_raw() -> Result(Nil, TerminalError) {
  set_raw(True)
  case enter_raw_ffi() {
    Error(e) -> Error(e)
    Ok(_) -> {
      let input_loop_pid = get_input_loop_pid()
      case input_loop_pid {
        Ok(pid) -> {
          process.send_exit(pid)
          start_input_loop()
          Ok(Nil)
        }
        Error(_) -> Error(FailedToEnterRawMode)
      }
    }
  }
}

/// Exits raw mode.
///
/// Raw mode is a mode where the terminal does not process input, but instead
/// passes it directly to the application. This means that:
/// - Input is not echoed to the screen
/// - Input is not line-buffered (characters are available immediately)
/// - Some special characters are not processed by the terminal
pub fn exit_raw() -> Result(Nil, TerminalError) {
  set_raw(False)
  exit_raw_ffi()
}

// it's kinda stupid, but i have to move internal input code to this file.
// reason: to correctly enter raw mode. because erlang_read_line blocks the thread,
// after entering raw mode you need to press enter once more after calling enter_raw()
// (see input_loop()). so i decided to just kill the input_loop() process and restart
// it. for that, i need to know and store its pid. but i can't restart input_loop()
// from tty.gleam without importing from input.gleam, which already imports from
// tty.gleam, and gleam doesn't allow circular dependencies.

@external(erlang, "tty_state", "save_input_loop_pid")
fn save_input_loop_pid(pid: Pid) -> Nil

@external(erlang, "tty_state", "get_input_loop_pid")
fn get_input_loop_pid() -> Result(Pid, Nil)

@external(erlang, "io", "get_chars")
fn get_chars(prompt: String, n: Int) -> String

@external(erlang, "io", "get_line")
fn erlang_read(prompt: String) -> String

@external(erlang, "input_ffi", "push")
fn push(event: Result(Event, EventError)) -> Nil

@internal
pub fn start_input_loop() {
  let input_loop_pid = process.spawn(fn() { input_loop() })
  save_input_loop_pid(input_loop_pid)
}

fn input_loop() {
  let str = case is_raw_mode() {
    True -> get_chars("", 128)
    False -> erlang_read("")
  }
  let str = string.to_graphemes(str)
  let events = parse_events(str, "", [], False)
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

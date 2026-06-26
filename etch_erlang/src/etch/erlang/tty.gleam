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
  todo
}

/// Exits raw mode.
///
/// Raw mode is a mode where the terminal does not process input, but instead
/// passes it directly to the application. This means that:
/// - Input is not echoed to the screen
/// - Input is not line-buffered (characters are available immediately)
/// - Some special characters are not processed by the terminal
pub fn exit_raw() -> Result(Nil, TerminalError) {
  todo
}
// it's kinda stupid, but i have to move internal input code to this file.
// reason: to correctly enter raw mode. because erlang_read_line blocks the thread,
// after entering raw mode you need to press enter once more after calling enter_raw()
// (see input_loop()). so i decided to just kill the input_loop() process and restart
// it. for that, i need to know and store its pid. but i can't restart input_loop()
// from tty.gleam without importing from input.gleam, which already imports from
// tty.gleam, and gleam doesn't allow circular dependencies.

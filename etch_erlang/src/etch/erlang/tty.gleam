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

@external(erlang, "input_ffi", "restart_or_start_input_loop")
fn restart_or_start_input_loop() -> Nil

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
  case enter_raw_ffi() {
    Ok(_) -> {
      set_raw(True)
      restart_or_start_input_loop()
      Ok(Nil)
    }
    Error(e) -> {
      Error(e)
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
  case exit_raw_ffi() {
    Ok(_) -> {
      set_raw(False)
      restart_or_start_input_loop()
      Ok(Nil)
    }
    Error(e) -> {
      Error(e)
    }
  }
}

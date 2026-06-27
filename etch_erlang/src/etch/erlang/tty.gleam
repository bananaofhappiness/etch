pub type TerminalError {
  CouldNotGetWindowSize
  FailedToEnterRawMode
  FailedToExitRawMode
}

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
@external(erlang, "terminal_ffi", "enter_raw")
pub fn enter_raw() -> Result(Nil, TerminalError)

/// Exits raw mode.
///
/// Raw mode is a mode where the terminal does not process input, but instead
/// passes it directly to the application. This means that:
/// - Input is not echoed to the screen
/// - Input is not line-buffered (characters are available immediately)
/// - Some special characters are not processed by the terminal
@external(erlang, "terminal_ffi", "exit_raw")
pub fn exit_raw() -> Result(Nil, TerminalError)

@external(erlang, "tty_state", "is_raw_mode")
pub fn is_raw_mode() -> Bool

/// Returns current window size.
@external(erlang, "terminal_ffi", "window_size")
pub fn window_size() -> Result(#(Int, Int), TerminalError)

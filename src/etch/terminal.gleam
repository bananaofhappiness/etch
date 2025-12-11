//// This module provides terminal associated functions like
//// entering raw mode, alternative screen, setting title etc.

import etch/internal/consts.{csi, esc}
import gleam/int

/// Used in [`clear`](terminal.html#clear) function.
pub type ClearType {
  /// Clears the whole screen.
  All
  /// Clears the whole screen and the history.
  Purge
  /// Clears cells from the cursor downwards.
  FromCursorDown
  /// Clears cells from the cursor upwards.
  FromCursorUp
  /// Clears cells at the current cursor row.
  CurrentLine
  /// Clears cells from the cursor positon until the end.
  UntilNewLine
}

/// Enters raw mode.
/// It is prefered not to use this directly. See [`EnterRaw`](command.html#EnterRaw).
@external(erlang, "terminal_ffi", "enter_raw")
pub fn enter_raw() -> Nil

/// Returns current window size.
@external(erlang, "terminal_ffi", "window_size")
pub fn window_size() -> #(Int, Int)

/// Clears the terminal. See [`ClearType`](terminal.html#ClearType).
/// It is prefered not to use this directly. See [`Clear`](command.html#Clear).
pub fn clear(t: ClearType) -> String {
  case t {
    All -> csi <> "2J"
    Purge -> csi <> "3J"
    FromCursorDown -> csi <> "J"
    FromCursorUp -> csi <> "1J"
    CurrentLine -> csi <> "2K"
    UntilNewLine -> csi <> "K"
  }
}

/// Sets terminal title.
/// It is prefered not to use this directly. See [`SetTitle`](command.html#SetTitle).
pub fn set_title(s: String) -> String {
  esc <> "]0;" <> s <> "\u{0007}"
}

/// Disable line wrap.
/// It is prefered not to use this directly. See [`DisableLineWrap`](command.html#DisableLineWrap).
pub fn disable_line_wrap() -> String {
  csi <> "?7l"
}

/// Enable line wrap.
/// It is prefered not to use this directly. See [`EnableLineWrap`](command.html#EnableLineWrap).
pub fn enable_line_wrap() -> String {
  csi <> "?7h"
}

/// Scroll N rows up.
/// It is prefered not to use this directly. See [`ScrollUp`](command.html#ScrollUp).
pub fn scroll_up(n: Int) -> String {
  csi <> int.to_string(n) <> "S"
}

/// Scroll N rows down.
/// It is prefered not to use this directly. See [`ScrollDown`](command.html#ScrollDown).
pub fn scroll_down(n: Int) -> String {
  csi <> int.to_string(n) <> "T"
}

/// Enter alternative screen.
/// It is prefered not to use this directly. See [`EnterAlternative`](command.html#EnterAlternative).
pub fn enter_alternative() -> String {
  csi <> "?1049h"
}

/// Leave alternative screen.
/// It is prefered not to use this directly. See [`LeaveAlternative`](command.html#LeaveAlternative).
pub fn leave_alternative() -> String {
  csi <> "?1049l"
}

/// Set window size. It does not work on most modern terminals
/// due to security issues.
/// It is prefered not to use this directly. See [`SetSize`](command.html#SetSize).
pub fn set_size(x: Int, y: Int) -> String {
  csi <> "8;" <> int.to_string(x) <> ";" <> int.to_string(y) <> "t"
}

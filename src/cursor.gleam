//// This module provides cursor associated functions like moving it,
//// hiding it or setting its style.

import gleam/int

const esc = "\u{001b}"

const csi = "\u{001b}["

/// Cursor style.
pub type CursorStyle {
  /// Default cursor shape.
  DefaultShape
  /// Blinking block █.
  BlinkingBlock
  /// Steady (non-blinking) block █.
  SteadyBlock
  /// Blinking underscore _.
  BlinkingUnderScore
  /// Steady (non-blinking) underscore _.
  SteadyUnderScore
  /// Blinking bar |.
  BlinkingBar
  /// Steady (non-blinking) bar |.
  SteadyBar
}

/// Moves cursor to the given position.
/// It is prefered not to use this directly. See [`MoveTo`](command.html#MoveTo).
pub fn move_to(x: Int, y: Int) -> String {
  csi <> int.to_string(y + 1) <> ";" <> int.to_string(x + 1) <> "H"
}

/// Hides cursor.
/// It is prefered not to use this directly. See [`Hide`](command.html#Hide).
pub fn hide() -> String {
  csi <> "?25l"
}

/// Shows cursor.
/// It is prefered not to use this directly. See [`Show`](command.html#Show).
pub fn show() -> String {
  csi <> "?25h"
}

/// Moves cursor to the next line (at the beginning of the line below).
/// It is prefered not to use this directly. See [`MoveToNextLine`](command.html#MoveToNextLine).
pub fn move_to_next_line(n: Int) -> String {
  csi <> int.to_string(n) <> "E"
}

/// Moves cursor to the previous line (at the beginning of the line above).
/// It is prefered not to use this directly. See [`MoveToPreviousLine`](command.html#MoveToPreviousLine).
pub fn move_to_previous_line(n: Int) -> String {
  csi <> int.to_string(n) <> "F"
}

/// Moves cursor to the given column (the row remains unchainged).
/// It is prefered not to use this directly. See [`MoveToColumn`](command.html#MoveToColumn).
pub fn move_to_column(n: Int) -> String {
  csi <> int.to_string(n) <> "G"
}

/// Moves cursor to the given row (the column remains unchainged).
/// It is prefered not to use this directly. See [`MoveToRow`](command.html#MoveToRow).
pub fn move_to_row(n: Int) -> String {
  csi <> int.to_string(n) <> "d"
}

/// Moves cursor n column to the right.
/// It is prefered not to use this directly. See [`MoveRight`](command.html#MoveRight).
pub fn move_right(n: Int) -> String {
  csi <> int.to_string(n) <> "C"
}

/// Moves cursor n column to the left.
/// It is prefered not to use this directly. See [`MoveLeft`](command.html#MoveLeft).
pub fn move_left(n: Int) -> String {
  csi <> int.to_string(n) <> "D"
}

/// Moves cursor n rows to the up.
/// It is prefered not to use this directly. See [`MoveUp`](command.html#MoveUp).
pub fn move_up(n: Int) -> String {
  csi <> int.to_string(n) <> "A"
}

/// Moves cursor n rows to the down.
/// It is prefered not to use this directly. See [`MoveDown`](command.html#MoveDown).
pub fn move_down(n: Int) -> String {
  csi <> int.to_string(n) <> "B"
}

/// Sets cursor style.
/// It is prefered not to use this directly. See [`SetCursorStyle`](command.html#SetCursorStyle).
pub fn set_cursor_style(s: CursorStyle) -> String {
  case s {
    DefaultShape -> csi <> "0 q"
    BlinkingBlock -> csi <> "1 q"
    SteadyBlock -> csi <> "2 q"
    BlinkingUnderScore -> csi <> "3 q"
    SteadyUnderScore -> csi <> "4 q"
    BlinkingBar -> csi <> "5 q"
    SteadyBar -> csi <> "6 q"
  }
}

/// Saves current cursor position.
/// It is prefered not to use this directly. See [`SavePosition`](command.html#SavePosition).
pub fn save_position() {
  esc <> "7"
}

/// Restores saved cursor position.
/// It is prefered not to use this directly. See [`RestorePosition`](command.html#RestorePosition).
pub fn restore_position() {
  esc <> "8"
}

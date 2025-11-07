import esc.{csi, esc}
import gleam/int

pub type ClearType {
  All
  Purge
  FromCursorDown
  FromCursorUp
  CurrentLine
  UntilNewLine
}

@external(erlang, "terminal_ffi", "enter_raw")
pub fn enter_raw() -> Nil

@external(erlang, "terminal_ffi", "window_size")
pub fn window_size() -> #(Int, Int)

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

pub fn set_title(s: String) -> String {
  esc <> "]0;" <> s <> "\u{0007}"
}

pub fn disable_line_wrap() -> String {
  csi <> "?7l"
}

pub fn enable_line_wrap() -> String {
  csi <> "?7h"
}

pub fn scroll_up(n: Int) -> String {
  csi <> int.to_string(n) <> "S"
}

pub fn scroll_down(n: Int) -> String {
  csi <> int.to_string(n) <> "T"
}

pub fn enter_alternative() -> String {
  csi <> "?1049h"
}

pub fn leave_alternative() -> String {
  csi <> "?1049l"
}

pub fn set_size(x: Int, y: Int) -> String {
  csi <> "8;" <> int.to_string(x) <> ";" <> int.to_string(y) <> "t"
}

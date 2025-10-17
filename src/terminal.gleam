import esc.{esc}
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

pub fn clear(t: ClearType) {
  case t {
    All -> esc("[2J")
    Purge -> esc("[3J")
    FromCursorDown -> esc("[J")
    FromCursorUp -> esc("[1J")
    CurrentLine -> esc("[2K")
    UntilNewLine -> esc("[K")
  }
}

pub fn enter_alternative() -> String {
  esc("[?1049h")
}

pub fn leave_alternative() -> String {
  esc("[?1049l")
}

pub fn set_size(x: Int, y: Int) -> String {
  esc("[8;") <> int.to_string(x) <> ";" <> int.to_string(y) <> "t"
}

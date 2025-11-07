import esc.{csi, esc}
import gleam/int

pub type CursorStyle {
  DefaultUserShape
  BlinkingBlock
  SteadyBlock
  BlinkingUnderScore
  SteadyUnderScore
  BlinkingBar
  SteadyBar
}

pub fn move_to(x: Int, y: Int) -> String {
  csi <> int.to_string(y + 1) <> ";" <> int.to_string(x + 1) <> "H"
}

pub fn hide() -> String {
  csi <> "?25l"
}

pub fn show() -> String {
  csi <> "?25h"
}

pub fn move_to_next_line(n: Int) -> String {
  csi <> int.to_string(n) <> "E"
}

pub fn move_to_previous_line(n: Int) -> String {
  csi <> int.to_string(n) <> "F"
}

pub fn move_to_column(n: Int) -> String {
  csi <> int.to_string(n) <> "G"
}

pub fn move_to_row(n: Int) -> String {
  csi <> int.to_string(n) <> "d"
}

pub fn move_right(n: Int) -> String {
  csi <> int.to_string(n) <> "C"
}

pub fn move_left(n: Int) -> String {
  csi <> int.to_string(n) <> "D"
}

pub fn move_up(n: Int) -> String {
  csi <> int.to_string(n) <> "A"
}

pub fn move_down(n: Int) -> String {
  csi <> int.to_string(n) <> "B"
}

pub fn set_cursor_style(s: CursorStyle) -> String {
  case s {
    DefaultUserShape -> csi <> "0 q"
    BlinkingBlock -> csi <> "1 q"
    SteadyBlock -> csi <> "2 q"
    BlinkingUnderScore -> csi <> "3 q"
    SteadyUnderScore -> csi <> "4 q"
    BlinkingBar -> csi <> "5 q"
    SteadyBar -> csi <> "6 q"
  }
}

pub fn save_position() {
  esc <> "7"
}

pub fn restore_position() {
  esc <> "8"
}

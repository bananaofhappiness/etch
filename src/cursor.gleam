import esc.{esc}
import gleam/int

pub fn move_to(x: Int, y: Int) -> String {
  esc("[") <> int.to_string(x) <> ";" <> int.to_string(y) <> "H"
}

pub fn hide() -> String {
  esc("[?25l")
}

pub fn show() -> String {
  esc("[?25h")
}

pub fn move_to_next_line(n: Int) -> String {
  esc("[") <> int.to_string(n) <> "E"
}

pub fn move_to_previous_line(n: Int) -> String {
  esc("[") <> int.to_string(n) <> "F"
}

pub fn move_to_column(n: Int) -> String {
  esc("[") <> int.to_string(n) <> "G"
}

pub fn move_to_row(n: Int) -> String {
  todo
  // esc("[") <> int.to_string(n) <> "G"
}

pub fn move_right(n: Int) -> String {
  esc("[") <> int.to_string(n) <> "C"
}

pub fn move_left(n: Int) -> String {
  esc("[") <> int.to_string(n) <> "D"
}

pub fn move_up(n: Int) -> String {
  esc("[") <> int.to_string(n) <> "A"
}

pub fn move_down(n: Int) -> String {
  esc("[") <> int.to_string(n) <> "B"
}

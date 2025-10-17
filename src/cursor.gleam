import esc.{esc}
import gleam/int
import gleam/io.{print}

pub fn set_position(x: Int, y: Int) -> String {
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

import consts.{esc}
import gleam/int
import gleam/io.{print}

pub fn set_position(x: Int, y: Int) {
  print(esc <> "[" <> int.to_string(x) <> ";" <> int.to_string(y) <> "H")
}

pub fn hide() {
  print(esc <> "[?25l")
}

pub fn show() {
  print(esc <> "[?25h")
}
